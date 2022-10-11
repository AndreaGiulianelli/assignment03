package pluviometer

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.*

object Pluviometer:
  enum Command:
    case TODO

  def apply(): Behavior[Command] = Behaviors.empty
