package firestation

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import model.CityModel.{FirestationService, Zone}
import util.Message

/*
  todo: gui is launched after the firestation received ok from the zone. Inoltre fare in modo che subito dopo la zona invii anche il suo stato così al volo in modo tale che si setti.
 */

object Firestation:
  sealed trait Command extends Message
  case class UpdateZoneStatus(zone: Zone) extends Command
  case class SearchZoneResult(listing: Receptionist.Listing) extends Command
  private case class FirestationStatus(firestation: FirestationService)
      extends Command //todo: whenever send this message all must be updated
  case object AlarmUnderManagement extends Command
  case object AlarmResolved extends Command

  sealed trait ZoneMessage extends Message
  case object UnderManagement extends ZoneMessage
  case object AlarmSolved extends ZoneMessage
  case class FirestationRegistrationResponse(zoneRef: ActorRef[ZoneMessage]) extends ZoneMessage

  def apply(firestation: FirestationService): Behavior[Command] = Behaviors.empty
