package controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import view.ViewActor

object Coordinator:
  enum Command:
    case Start
    case Stop
    case Resume
    case UpdatedPositions(updatedPositions: Simulation.Message.Update)
    case Terminated

  def apply(viewActor: ActorRef[ViewActor.Command]): Behavior[Command] = Behaviors.setup { ctx =>
    //new Coordinator(viewActor).???
    ???
  }

case class Coordinator(viewActor: ActorRef[ViewActor.Command]):
  ???
