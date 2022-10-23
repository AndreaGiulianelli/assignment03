package firestation

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import model.CityModel.{ALARM, BUSY, FREE, FirestationService, Zone}
import util.Message
import zone.ZoneControl
import util.Utils.AkkaUtils.*

object Firestation:
  private val firestationServiceKey = ServiceKey[Firestation.Command]("firestation-servicekey")

  sealed trait Command extends Message
  case object Start extends Command
  private case class SearchZoneResult(listing: Receptionist.Listing) extends Command
  private case class FirestationUpdate(ref: ActorRef[Command], firestation: FirestationService) extends Command
  private case class FirestationListUpdate(listing: Receptionist.Listing) extends Command
  case object AlarmUnderManagement extends Command
  case object AlarmSolved extends Command
  case class UpdateZoneStatus(zone: Zone) extends Command
  case class FirestationRegistrationResponse(zoneRef: ActorRef[ZoneMessage], accepted: Boolean) extends Command

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
        case FirestationRegistrationResponse(zoneRef, true) =>
          // Now the firestation is associated with the zone, so the firestation can effectively start
          ctx.system.receptionist ! Receptionist.register(firestationServiceKey, ctx.self)
          ctx.system.receptionist ! Receptionist.Subscribe(
            firestationServiceKey,
            ctx.messageAdapter[Receptionist.Listing](FirestationListUpdate.apply)
          )
          // Start also the dashboard
          val viewActor = ctx.spawn(FirestationViewActor(), "viewer")
          viewActor ! FirestationViewActor.Command.Start(firestation.associatedZone, 400, 400, ctx.self)
          FirestationActor(zoneRef, viewActor).free(firestation) // start the effective behavior of the firestation
        case FirestationRegistrationResponse(_, false) =>
          idle(firestation) // if it's not accepted by the zone, return to idle
        case _ => Behaviors.unhandled
    }

  private case class FirestationActor(
      zoneRef: ActorRef[ZoneMessage],
      viewer: ActorRef[FirestationViewActor.Command],
      firestations: Set[ActorRef[Command]] = Set.empty
  ):
    import monocle.syntax.all._

    def free(firestation: FirestationService): Behavior[Command] = Behaviors.receivePartial {
      handleUpdates(firestation).orElse { case (ctx, AlarmUnderManagement) =>
        if firestation.associatedZone.status == ALARM() then
          val updatedFirestation = firestation.focus(_.status).replace(BUSY())
          zoneRef ! UnderManagement
          firestations ! FirestationUpdate(ctx.self, updatedFirestation)
          viewer ! FirestationViewActor.Command.UpdateFirestation(updatedFirestation)
          busy(updatedFirestation)
        else Behaviors.same
      }
    }

    private def busy(firestation: FirestationService): Behavior[Command] = Behaviors.receivePartial {
      handleUpdates(firestation).orElse { case (ctx, AlarmSolved) =>
        val updatedFirestation = firestation.focus(_.status).replace(FREE())
        zoneRef ! Solved
        firestations ! FirestationUpdate(ctx.self, updatedFirestation)
        viewer ! FirestationViewActor.Command.UpdateFirestation(updatedFirestation)
        free(updatedFirestation)
      }
    }

    private def handleUpdates(
        firestation: FirestationService
    ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] =
      def _changeState(
          actor: FirestationActor,
          firestation: FirestationService
      ): Behavior[Command] =
        if firestation.status == FREE() then actor.free(firestation)
        else actor.busy(firestation)

      {
        case (ctx, FirestationListUpdate(firestationServiceKey.Listing(list))) =>
          // update the known firestations (excluding myself)
          val updatedActor = this.focus(_.firestations).replace(list.filter(_ != ctx.self))
          // send data to new firestations
          updatedActor.firestations.diff(firestations) ! FirestationUpdate(ctx.self, firestation)
          _changeState(updatedActor, firestation)
        case (_, FirestationUpdate(_, station)) =>
          viewer ! FirestationViewActor.Command.UpdateFirestation(station)
          _changeState(this, firestation)
        case (ctx, UpdateZoneStatus(zone)) =>
          val updatedFirestation = firestation.focus(_.associatedZone).replace(zone)
          firestations ! FirestationUpdate(ctx.self, updatedFirestation) // update other firestations
          viewer ! FirestationViewActor.Command.UpdateFirestation(updatedFirestation)
          _changeState(this, updatedFirestation)
      }
