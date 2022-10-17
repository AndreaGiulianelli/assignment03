package zone

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.cluster.typed.{Cluster, Subscribe}
import akka.cluster.ClusterEvent.MemberExited
import pluviometer.Pluviometer
import firestation.Firestation
import model.CityModel.{ALARM, NORMAL, UNDER_MANAGEMENT, Zone, ZoneStatus}
import pluviometer.Pluviometer.DataResponse
import util.Message
import util.Utils.AkkaUtils.*
import monocle.syntax.all.*

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
      .orElse { case (ctx, Alarm(pluviometer)) =>
        zoneState.pluviometers ! Pluviometer.GetStatus(
          ctx.messageAdapter[Pluviometer.DataResponse](PluviometerStatus.apply)
        )
        ctx.log.info(s"------------ ZONE ${zoneState.zone.zoneId} RECEIVED FIRST ALARM - CHECK --------------")
        Behaviors.withStash(100) { stash =>
          preAlarm(stash, zoneState, Map(pluviometer -> true))
        }
      }
  }

  private def preAlarm(
      stash: StashBuffer[Command],
      zoneState: ZoneState,
      alarmMap: Map[ActorRef[Pluviometer.Command], Boolean]
  ): Behavior[Command] =
    def _check(
        updateAlarmMap: Map[ActorRef[Pluviometer.Command], Boolean],
        zoneToCheck: ZoneState = zoneState
    ): Behavior[Command] =
      if updateAlarmMap.keys.size >= zoneToCheck.pluviometers.size then
        if updateAlarmMap
            .filter((k, v) => zoneToCheck.pluviometers.contains(k) && v)
            .size >= Math.floor(zoneToCheck.pluviometers.size / 2) + 1
        then
          println(s"------------ ZONE ${zoneToCheck.zone.zoneId} ALAAAARMMMMMM --------------")
          val updatedZoneState = zoneToCheck.focus(_.zone).modify(_.focus(_.status).replace(ALARM()))
          updatedZoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
          stash.unstashAll(alarm(updatedZoneState))
        else
          println(s"------------ ZONE ${zoneToCheck.zone.zoneId} FALSE ALARM --------------")
          stash.unstashAll(normal(zoneToCheck))
      else preAlarm(stash, zoneToCheck, updateAlarmMap)

    Behaviors.receivePartial {
      handleFirestationRegistration(zoneState, z => preAlarm(stash, z, alarmMap))
        .orElse(handlePluviometerExit(zoneState, z => _check(alarmMap, z)))
        .orElse { (ctx, msg) =>
          msg match
            case PluviometerStatus(Pluviometer.Status(p, isInAlarm)) =>
              val updateAlarmMap = alarmMap + (p -> isInAlarm)
              ctx.log.info(
                s"------------ ZONE ${zoneState.zone.zoneId} RECEIVED OTHER STUFF ${p -> isInAlarm} - CHECK --------------"
              )
              _check(updateAlarmMap)
            case Alarm(p) =>
              val updateAlarmMap = alarmMap + (p -> true)
              ctx.log.info(
                s"------------ ZONE ${zoneState.zone.zoneId} RECEIVED OTHER STUFF ${p -> true} - CHECK --------------"
              )
              _check(updateAlarmMap)
            case other =>
              stash.stash(other)
              Behaviors.same
        }
    }

  private def alarm(
      zoneState: ZoneState
  ): Behavior[Command] = Behaviors.receivePartial {
    handlePluviometerRegistration(zoneState, alarm)
      .orElse(handleFirestationRegistration(zoneState, alarm))
      .orElse(handlePluviometerExit(zoneState, alarm))
      .orElse { case (ctx, FirestationUpdate(update)) =>
        update match
          case Firestation.UnderManagement =>
            val updatedZoneState =
              zoneState.focus(_.zone).modify(_.focus(_.status).replace(UNDER_MANAGEMENT()))
            ctx.log.info(s"------------ ZONE ${zoneState.zone.zoneId} UNDER MANAGEMENT --------------")
            updatedZoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
            alarmUnderManagement(updatedZoneState)
      }
  }

  private def alarmUnderManagement(
      zoneState: ZoneState
  ): Behavior[Command] = Behaviors.receivePartial {
    handlePluviometerRegistration(zoneState, alarmUnderManagement)
      .orElse(handlePluviometerExit(zoneState, alarmUnderManagement))
      .orElse { case (ctx, FirestationUpdate(update)) =>
        update match
          case Firestation.Solved =>
            ctx.log.info(s"------------ ZONE ${zoneState.zone.zoneId} SOOOOLVEDDDD --------------")
            val updatedZoneState = zoneState.focus(_.zone).modify(_.focus(_.status).replace(NORMAL()))
            updatedZoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
            normal(updatedZoneState)
      }
  }

  // --- HANDLER FOR NODES ---
  private def handlePluviometerRegistration(
      zoneState: ZoneState,
      nextState: ZoneState => Behavior[Command]
  ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] = { case (ctx, RegisterPluviometer(ref)) =>
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
  ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] = {
    case (_, PluviometerExit(MemberExited(member))) =>
      if zoneState.pluviometers.map(_.path.address).contains(member.address) then
        val updatedZoneState = zoneState
          .focus(_.pluviometers)
          .modify(_.filter(_.path.address != member.address))
          .focus(_.zone)
          .modify(_.focus(_.sensors).modify(_ - 1))
        zoneState.firestation.foreach(_ ! Firestation.UpdateZoneStatus(updatedZoneState.zone))
        nextState(updatedZoneState)
      else nextState(zoneState)
  }
  private def handleFirestationRegistration(
      zoneState: ZoneState,
      nextState: ZoneState => Behavior[Command]
  ): PartialFunction[(ActorContext[Command], Command), Behavior[Command]] = { case (ctx, RegisterFirestation(ref)) =>
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
