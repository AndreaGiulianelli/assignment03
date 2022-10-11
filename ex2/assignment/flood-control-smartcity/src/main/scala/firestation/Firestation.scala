package firestation

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.*

object Firestation:
  enum Command:
    case TODO

  def apply(): Behavior[Command] = Behaviors.empty
