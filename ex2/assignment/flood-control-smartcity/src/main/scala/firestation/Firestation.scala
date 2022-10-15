package firestation

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.*

/*
  todo: gui is launched after the firestation received ok from the zone. Inoltre fare in modo che subito dopo la zona invii anche il suo stato così al volo in modo tale che si setti.
 */

object Firestation:
  enum Command:
    case TODO //todo

  def apply(): Behavior[Command] = Behaviors.empty
