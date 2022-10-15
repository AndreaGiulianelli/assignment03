package pluviometer

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.*
import util.Message
import zone.ZoneControl

object Pluviometer:
  sealed trait Command extends Message
  private case object GenerateData extends Command
  case class GetStatus(replyTo: ActorRef[DataResponse])
  case class PluviometerRegistrationResponse(zoneRef: ActorRef[ZoneControl.Command])
  case class SearchZoneResult(listing: Receptionist.Listing) extends Command

  sealed trait DataResponse extends Message
  case class Status(alarm: Boolean) extends DataResponse

  def apply(): Behavior[Command] = Behaviors.empty
