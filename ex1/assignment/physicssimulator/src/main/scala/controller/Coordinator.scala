package controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import model.Boundary
import view.ViewActor

/** Module that model the Coordinator actor. */
object Coordinator:
  enum Command:
    case Start
    case Stop
    case Resume
    case Update(updatedPositions: Simulation.Message)

  def apply(
      viewActor: Option[ActorRef[ViewActor.Command]],
      iterations: Int,
      bodyNumber: Int,
      boundary: Boundary
  ): Behavior[Command] =
    Behaviors.setup { _ =>
      CoordinatorImpl(viewActor, iterations, bodyNumber, boundary).idle
    }

  private case class CoordinatorImpl(
      viewActor: Option[ActorRef[ViewActor.Command]],
      iterations: Int,
      bodyNumber: Int,
      boundary: Boundary
  ):
    import Coordinator.Command

    val idle: Behavior[Command] = Behaviors.receivePartial { (ctx, msg) =>
      msg match
        case Command.Start =>
          val adapter = ctx.messageAdapter(Command.Update.apply)
          val simulationActor = ctx.spawn(Simulation(adapter), "simulation")
          simulationActor ! Simulation.Command.Start(iterations, bodyNumber, boundary)
          going(simulationActor)
    }

    private def going(simulationActor: ActorRef[Simulation.Command]): Behavior[Command] = Behaviors.receivePartial {
      (ctx, msg) =>
        msg match
          case Command.Update(Simulation.Message.Update(iter, vt, positions)) =>
            viewActor.foreach(_ ! ViewActor.Command.Update(positions, vt, iter, boundary))
            Behaviors.same
          case Command.Update(Simulation.Message.Terminated) =>
            viewActor.foreach(_ ! ViewActor.Command.Terminated)
            ctx.system.terminate()
            Behaviors.stopped
          case Command.Stop =>
            simulationActor ! Simulation.Command.Stop
            stop(simulationActor)
    }

    private def stop(simulationActor: ActorRef[Simulation.Command]): Behavior[Command] = Behaviors.withStash(100) {
      stash =>
        Behaviors.receive { (_, msg) =>
          msg match
            case Command.Resume =>
              simulationActor ! Simulation.Command.Resume
              stash.unstashAll(going(simulationActor))
            case other =>
              stash.stash(other)
              Behaviors.same
        }
    }
