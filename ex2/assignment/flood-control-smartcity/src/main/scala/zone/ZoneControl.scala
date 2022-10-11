package zone

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.*

object ZoneControl:
  enum Command:
    case TODO

  def apply(): Behavior[Command] = Behaviors.empty
