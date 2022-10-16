package pluviometer

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.*
import model.CityModel.PluviometerSensor
import pluviometer.Pluviometer.PluviometerRegistrationResponse
import util.Message
import zone.ZoneControl

import concurrent.duration.DurationInt
import scala.util.Random

object Pluviometer:
  sealed trait Command extends Message
  private case object GenerateData extends Command
  case object Start extends Command
  case class GetStatus(replyTo: ActorRef[DataResponse]) extends Command
  case class PluviometerRegistrationResponse(zoneRef: ActorRef[ZoneControl.Command], accepted: Boolean) extends Command
  case class SearchZoneResult(listing: Receptionist.Listing) extends Command

  sealed trait DataResponse extends Message
  case class Status(ref: ActorRef[Command], alarm: Boolean) extends DataResponse

  def apply(pluviometer: PluviometerSensor): Behavior[Command] = idle(pluviometer)

  private def idle(pluviometer: PluviometerSensor): Behavior[Command] = Behaviors.receive { (ctx, msg) =>
    val zoneKey = ZoneControl.Service.serviceKey(pluviometer.associatedZone)
    msg match
      case Start =>
        val adapter = ctx.messageAdapter[Receptionist.Listing](SearchZoneResult.apply)
        ctx.system.receptionist ! Receptionist.Subscribe(zoneKey, adapter)
        ctx.log.info(
          s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} STARTED --------"
        )
        Behaviors.same
      case SearchZoneResult(zoneKey.Listing(list)) =>
        ctx.log.info(
          s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} OBTAINED RESULTS --------"
        )
        if list.nonEmpty then
          val zoneRef = list.head
          ctx.log.info(
            s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} REQUEST REGISTER --------"
          )
          zoneRef ! ZoneControl.RegisterPluviometer(ctx.self)
          pairing(pluviometer, zoneRef)
        else
          ctx.log.info(
            s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} EMPTY RESULTS --------"
          )
          Behaviors.same
      case _ => Behaviors.unhandled
  }

  private def pairing(pluviometer: PluviometerSensor, zoneRef: ActorRef[ZoneControl.Command]): Behavior[Command] =
    Behaviors.receive { (ctx, msg) =>
      msg match
        case PluviometerRegistrationResponse(ref, true) =>
          if zoneRef == ref then
            ctx.log.info(
              s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} REGISTERED --------"
            )
            working(pluviometer, zoneRef)
          else Behaviors.same
        case PluviometerRegistrationResponse(_, false) =>
          ctx.log.info(
            s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} FAILED REGISTER --------"
          )
          idle(pluviometer) // if it's not accepted by the zone, return to idle
        case _ => Behaviors.unhandled
    }

  private def working(pluviometer: PluviometerSensor, zoneRef: ActorRef[ZoneControl.Command]): Behavior[Command] =
    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay(GenerateData, pluviometer.senseRate.millis)
      Behaviors.receive { (ctx, msg) =>
        msg match
          case GenerateData =>
            ctx.log.info(
              s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} GENERATING.... --------"
            )
            if pluviometer.inAlarm then zoneRef ! ZoneControl.Alarm(ctx.self)
            Behaviors.same
          case GetStatus(ref) =>
            ctx.log.info(
              s"------- PLUVIOMETER ${pluviometer.associatedZone.zoneId}-${pluviometer.pluviometerId} ASKED STATUS --------"
            )
            ref ! Status(ctx.self, pluviometer.inAlarm)
            Behaviors.same
          case _ => Behaviors.unhandled
      }

    }
