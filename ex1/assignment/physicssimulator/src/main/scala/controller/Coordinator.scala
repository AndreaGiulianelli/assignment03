package controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import model.Boundary
import view.ViewActor

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
    Behaviors.setup { ctx =>
      new Coordinator(viewActor, iterations, bodyNumber, boundary).idle
    }

case class Coordinator(
    viewActor: Option[ActorRef[ViewActor.Command]],
    iterations: Int,
    bodyNumber: Int,
    boundary: Boundary
):
  import Coordinator.Command

  val idle: Behavior[Command] = Behaviors.receivePartial { (ctx, msg) =>
    msg match
      case Command.Start =>
        //ctx.log.info("COORDINATOR ACTOR: received start")
        val adapter = ctx.messageAdapter(Command.Update.apply)
        val simulationActor = ctx.spawn(Simulation(adapter), "simulation")
        simulationActor ! Simulation.Command.Start(iterations, bodyNumber, boundary)
        going(simulationActor)
  }

  private def going(simulationActor: ActorRef[Simulation.Command]): Behavior[Command] = Behaviors.receivePartial {
    case (ctx, Command.Update(Simulation.Message.Update(iter, vt, positions))) =>
      //ctx.log.info("COORDINATOR ACTOR: received update")
      viewActor.foreach(_ ! ViewActor.Command.Update(positions, vt, iter, boundary))
      Behaviors.same
    case (ctx, Command.Update(Simulation.Message.Terminated)) =>
      //ctx.log.info("COORDINATOR ACTOR: received TERMINATION")
      viewActor.foreach(_ ! ViewActor.Command.Terminated)
      Behaviors.stopped
    case (ctx, Command.Stop) =>
      //ctx.log.info("COORDINATOR ACTOR: received STOP")
      simulationActor ! Simulation.Command.Stop
      stop(simulationActor)
  }

  private def stop(simulationActor: ActorRef[Simulation.Command]): Behavior[Command] = Behaviors.withStash(100) {
    stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case Command.Resume =>
            //ctx.log.info("COORDINATOR ACTOR: received RESUME")
            simulationActor ! Simulation.Command.Resume
            stash.unstashAll(going(simulationActor))
          case other =>
            //ctx.log.info(s"COORDINATOR ACTOR (stop): received msg $other")
            stash.stash(other)
            Behaviors.same
      }
  }
