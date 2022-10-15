package zone

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.*
import akka.cluster.ClusterEvent.MemberExited
import pluviometer.Pluviometer
import firestation.Firestation
import util.Message

object ZoneControl:
  sealed trait Command extends Message
  private case class PluviometerStatus(status: Pluviometer.DataResponse) extends Command
  private case class FirestationUpdate(update: Firestation.ZoneMessage) extends Command
  case class Alarm(pluviometer: ActorRef[Pluviometer.Command]) extends Command
  case class RegisterPluviometer(pluviometer: ActorRef[Pluviometer.Command]) extends Command
  case class RegisterFirestation(firestation: ActorRef[Firestation.ZoneMessage]) extends Command
  // todo: check if I can insert directly MemberExit or I need Member Event
  case class PluviometerExit(event: MemberExited) extends Command

  def apply(): Behavior[Command] = Behaviors.empty
