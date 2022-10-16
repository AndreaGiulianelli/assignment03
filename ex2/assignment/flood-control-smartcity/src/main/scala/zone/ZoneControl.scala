package zone

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.cluster.typed.{Cluster, Subscribe}
import akka.cluster.ClusterEvent.MemberExited
import pluviometer.Pluviometer
import firestation.Firestation
import model.CityModel.Zone
import model.CityModel.ZoneStatus
import pluviometer.Pluviometer.DataResponse
import util.Message
import util.Utils.AkkaUtils.*
import monocle.syntax.all._

object ZoneControl:
  sealed trait Command extends Message
  private case class PluviometerStatus(status: Pluviometer.DataResponse) extends Command
  private case class FirestationUpdate(update: Firestation.ZoneMessage) extends Command
  case class Alarm(pluviometer: ActorRef[Pluviometer.Command]) extends Command
  case class RegisterPluviometer(pluviometer: ActorRef[Pluviometer.Command]) extends Command
  case class RegisterFirestation(firestation: ActorRef[Firestation.Command]) extends Command
  case class PluviometerExit(event: MemberExited) extends Command

  private case class ZoneState(
      zone: Zone,
      pluviometers: Set[ActorRef[Pluviometer.Command]],
      firestation: Option[ActorRef[Firestation.Command]]
  )

  def apply(zone: Zone): Behavior[Command] = Behaviors.setup { ctx =>
    ctx.system.receptionist ! Receptionist.register(Service.serviceKey(zone), ctx.self)
    Cluster(ctx.system).subscriptions ! Subscribe(
      ctx.messageAdapter[MemberExited](PluviometerExit.apply),
      classOf[MemberExited]
    )
    normal(ZoneState(zone, Set.empty, None))
  }

  private def normal(
      zoneState: ZoneState
  ): Behavior[Command] = Behaviors.receivePartial {
    handlePluviometerRegistration(zoneState, normal)
      .orElse(handleFirestationRegistration(zoneState, normal))
      .orElse(handlePluviometerExit(zoneState, normal))
      .orElse { (ctx, msg) =>
        msg match
          case Alarm(pluviometer) =>
            zoneState.pluviometers ! Pluviometer.GetStatus(
              ctx.messageAdapter[Pluviometer.DataResponse](PluviometerStatus.apply)
            )
            preAlarm(zoneState, Map(pluviometer -> true))
      }
  }

  private def preAlarm(
      zoneState: ZoneState,
      alarmMap: Map[ActorRef[Pluviometer.Command], Boolean]
  ): Behavior[Command] =
    def _check(
        stash: StashBuffer[Command],
        updateAlarmMap: Map[ActorRef[Pluviometer.Command], Boolean]
    ): Behavior[Command] =
      if updateAlarmMap.keys.size >= zoneState.pluviometers.size then
        if updateAlarmMap
            .filter((k, v) => zoneState.pluviometers.contains(k) && v)
            .size > zoneState.pluviometers.size / 2
        then
          val updatedZoneState = zoneState.focus(_.zone).modify(_.focus(_.status).replace(ZoneStatus.ALARM))
          updatedZoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
          stash.unstashAll(alarm(updatedZoneState))
        else stash.unstashAll(normal(zoneState))
      else preAlarm(zoneState, updateAlarmMap)

    Behaviors.receivePartial {
      handleFirestationRegistration(zoneState, z => preAlarm(z, alarmMap = alarmMap))
        .orElse(handlePluviometerExit(zoneState, z => preAlarm(z, alarmMap = alarmMap)))
        .orElse((_, msg) =>
          Behaviors.withStash(100) { stash =>
            msg match
              case PluviometerStatus(Pluviometer.Status(p, isInAlarm)) =>
                val updateAlarmMap = alarmMap + (p -> isInAlarm)
                _check(stash, updateAlarmMap)
              case Alarm(p) =>
                val updateAlarmMap = alarmMap + (p -> true)
                _check(stash, updateAlarmMap)
              case other =>
                stash.stash(other)
                Behaviors.same
          }
        )
    }

  private def alarm(
      zoneState: ZoneState
  ): Behavior[Command] = Behaviors.receivePartial {
    handlePluviometerRegistration(zoneState, alarm)
      .orElse(handleFirestationRegistration(zoneState, alarm))
      .orElse(handlePluviometerExit(zoneState, alarm))
      .orElse { (_, msg) =>
        msg match
          case FirestationUpdate(update) =>
            update match
              case Firestation.UnderManagement =>
                val updatedZoneState =
                  zoneState.focus(_.zone).modify(_.focus(_.status).replace(ZoneStatus.UNDER_MANAGEMENT))
                updatedZoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
                alarmUnderManagement(updatedZoneState)
      }
  }

  private def alarmUnderManagement(
      zoneState: ZoneState
  ): Behavior[Command] = Behaviors.receivePartial {
    handlePluviometerRegistration(zoneState, alarmUnderManagement)
      .orElse(handlePluviometerExit(zoneState, alarmUnderManagement))
      .orElse { (_, msg) =>
        msg match
          case FirestationUpdate(update) =>
            update match
              case Firestation.Solved =>
                val updatedZoneState = zoneState.focus(_.zone).modify(_.focus(_.status).replace(ZoneStatus.NORMAL))
                updatedZoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
                normal(updatedZoneState)
      }
  }

  // --- HANDLER FOR NODES ---
  private def handlePluviometerRegistration(
      zoneState: ZoneState,
      nextState: ZoneState => Behavior[Command]
  ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] = { (ctx, msg) =>
    msg match
      case RegisterPluviometer(ref) =>
        ref ! Pluviometer.PluviometerRegistrationResponse(ctx.self, true)
        val updatedZoneState = zoneState
          .focus(_.pluviometers)
          .modify(_ + ref)
          .focus(_.zone)
          .modify(_.focus(_.sensors).modify(_ + 1))
        zoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
        nextState(updatedZoneState)
  }
  private def handlePluviometerExit(
      zoneState: ZoneState,
      nextState: ZoneState => Behavior[Command]
  ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] = { (_, msg) =>
    msg match
      case PluviometerExit(MemberExited(member)) =>
        val updatedZoneState = zoneState
          .focus(_.pluviometers)
          .modify(_.filter(_.path.address != member.address))
          .focus(_.zone)
          .modify(_.focus(_.sensors).modify(_ - 1))
        zoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
        nextState(updatedZoneState)
  }
  private def handleFirestationRegistration(
      zoneState: ZoneState,
      nextState: ZoneState => Behavior[Command]
  ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] = { (ctx, msg) =>
    msg match
      case RegisterFirestation(ref) =>
        ref ! Firestation.FirestationRegistrationResponse(
          ctx.messageAdapter[Firestation.ZoneMessage](FirestationUpdate.apply),
          true
        )
        ref ! Firestation.UpdateZoneStatus(zoneState.zone) // send immediately the current state to the firestation
        nextState(zoneState.focus(_.firestation).replace(Some(ref)))
  }

  object Service:
    import akka.actor.typed.receptionist.ServiceKey
    def serviceKey(zone: Zone): ServiceKey[ZoneControl.Command] =
      ServiceKey[ZoneControl.Command](s"zone${zone.zoneId}")
