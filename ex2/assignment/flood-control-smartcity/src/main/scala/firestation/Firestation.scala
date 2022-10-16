package firestation

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import model.CityModel.{ALARM, BUSY, FREE, FirestationService, FirestationStatus, Zone, ZoneStatus}
import util.Message
import zone.ZoneControl
import util.Utils.AkkaUtils.*

/*
  todo: gui is launched after the firestation received ok from the zone. Inoltre fare in modo che subito dopo la zona invii anche il suo stato così al volo in modo tale che si setti.
 */

object Firestation:
  val firestationServiceKey = ServiceKey[Firestation.Command]("firestation-servicekey")

  sealed trait Command extends Message
  case object Start extends Command
  case class SearchZoneResult(listing: Receptionist.Listing) extends Command
  private case class FirestationUpdate(ref: ActorRef[Command], firestation: FirestationService)
      extends Command //todo: whenever send this message all must be updated
  case object AlarmUnderManagement extends Command
  case object AlarmSolved extends Command
  case class UpdateZoneStatus(zone: Zone) extends Command // todo: update dell'oggetto in firestationService
  case class FirestationRegistrationResponse(zoneRef: ActorRef[ZoneMessage], accepted: Boolean) extends Command
  case class FirestationListUpdate(listing: Receptionist.Listing) extends Command

  sealed trait ZoneMessage extends Message
  case object UnderManagement extends ZoneMessage
  case object Solved extends ZoneMessage

  def apply(firestation: FirestationService): Behavior[Command] = idle(firestation)

  private def idle(firestation: FirestationService): Behavior[Command] = Behaviors.receive { (ctx, msg) =>
    val zoneKey = ZoneControl.Service.serviceKey(firestation.associatedZone)
    msg match
      case Start =>
        val adapter = ctx.messageAdapter[Receptionist.Listing](SearchZoneResult.apply)
        ctx.system.receptionist ! Receptionist.Subscribe(zoneKey, adapter)
        Behaviors.same
      case SearchZoneResult(zoneKey.Listing(list)) =>
        if list.nonEmpty then
          list.head ! ZoneControl.RegisterFirestation(ctx.self)
          pairing(firestation)
        else Behaviors.same
      case _ => Behaviors.unhandled
  }

  private def pairing(firestation: FirestationService): Behavior[Command] =
    Behaviors.receive { (ctx, msg) =>
      msg match
        case FirestationRegistrationResponse(ref, true) =>
          ctx.system.receptionist ! Receptionist.register(firestationServiceKey, ctx.self)
          ctx.system.receptionist ! Receptionist.Subscribe(
            firestationServiceKey,
            ctx.messageAdapter[Receptionist.Listing](FirestationListUpdate.apply)
          )
          val viewActor = ctx.spawn(FirestationViewActor(), "viewer")
          viewActor ! FirestationViewActor.Command.Start(firestation.associatedZone, 400, 400, ctx.self)
          FirestationActor(ref, viewActor).free(firestation)
        case FirestationRegistrationResponse(ref, false) =>
          idle(firestation) // if it's not accepted by the zone, return to idle
        case _ => Behaviors.unhandled
    }

  private case class FirestationActor(
      zoneRef: ActorRef[ZoneMessage],
      viewer: ActorRef[FirestationViewActor.Command],
      firestations: Set[ActorRef[Command]] = Set.empty
  ):
    import monocle.syntax.all._

    def free(
        firestation: FirestationService,
        statuses: Map[ActorRef[Firestation.Command], FirestationService] = Map.empty
    ): Behavior[Command] =
      Behaviors.receivePartial {
        handleUpdates(firestation, statuses).orElse { (ctx, msg) =>
          msg match
            case AlarmUnderManagement =>
              if firestation.associatedZone.status == ALARM() then
                val updatedFirestation = firestation.focus(_.status).replace(BUSY())
                firestations ! FirestationUpdate(ctx.self, updatedFirestation)
                zoneRef ! UnderManagement
                viewer ! FirestationViewActor.Command.UpdateFirestation(updatedFirestation)
                busy(updatedFirestation, statuses)
              else Behaviors.same
            case _ => Behaviors.unhandled
        }
      }

    private def busy(
        firestation: FirestationService,
        statuses: Map[ActorRef[Firestation.Command], FirestationService] = Map.empty
    ): Behavior[Command] = Behaviors.receivePartial {
      handleUpdates(firestation, statuses).orElse { (ctx, msg) =>
        msg match
          case AlarmSolved =>
            val updatedFirestation = firestation.focus(_.status).replace(FREE())
            firestations ! FirestationUpdate(ctx.self, updatedFirestation)
            zoneRef ! Solved
            viewer ! FirestationViewActor.Command.UpdateFirestation(updatedFirestation)
            free(updatedFirestation, statuses)
          case _ => Behaviors.unhandled
      }
    }

    private def handleUpdates(
        firestation: FirestationService,
        statuses: Map[ActorRef[Firestation.Command], FirestationService] = Map.empty
    ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] =
      def _changeState(
          actor: FirestationActor,
          firestation: FirestationService,
          statuses: Map[ActorRef[Firestation.Command], FirestationService]
      ): Behavior[Command] =
        if firestation.status == FREE() then actor.free(firestation, statuses)
        else actor.busy(firestation, statuses)

      (ctx, msg) =>
        msg match
          case FirestationListUpdate(firestationServiceKey.Listing(list)) =>
            val updatedActor = this.focus(_.firestations).replace(list)
            _changeState(updatedActor, firestation, statuses)
          case FirestationUpdate(ref, station) =>
            val updatedStatuses = statuses + (ref -> station)
            viewer ! FirestationViewActor.Command.UpdateFirestation(station)
            _changeState(this, firestation, updatedStatuses)
          case UpdateZoneStatus(zone) =>
            val updatedFirestation = firestation.focus(_.associatedZone).replace(zone)
            firestations ! FirestationUpdate(ctx.self, updatedFirestation) // update other firestations
            viewer ! FirestationViewActor.Command.UpdateFirestation(updatedFirestation)
            _changeState(this, updatedFirestation, statuses)
