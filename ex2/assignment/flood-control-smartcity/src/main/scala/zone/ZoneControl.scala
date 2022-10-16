package zone

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.*
import akka.cluster.ClusterEvent.MemberExited
import pluviometer.Pluviometer
import firestation.Firestation
import model.CityModel.Zone
import util.Message

object ZoneControl:
  sealed trait Command extends Message
  private case class PluviometerStatus(status: Pluviometer.DataResponse) extends Command
  private case class FirestationUpdate(update: Firestation.ZoneMessage) extends Command
  case class Alarm(pluviometer: ActorRef[Pluviometer.Command]) extends Command
  case class RegisterPluviometer(pluviometer: ActorRef[Pluviometer.Command]) extends Command
  case class RegisterFirestation(firestation: ActorRef[Firestation.Command]) extends Command
  // todo: check if I can insert directly MemberExit or I need Member Event
  case class PluviometerExit(event: MemberExited) extends Command

  def apply(zone: Zone): Behavior[Command] = Behaviors.empty

  object Service:
    import akka.actor.typed.receptionist.ServiceKey
    def serviceKey(zone: Zone): ServiceKey[ZoneControl.Command] =
      ServiceKey[ZoneControl.Command](s"zone${zone.zoneId}")
