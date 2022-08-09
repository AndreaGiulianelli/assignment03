package controller

import model.{Body, Boundary, P2d, V2d}
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import controller.Simulation.{DEFAULT_MASS, DELTA_TIME}

import scala.util.Random

object Simulation:
  private val DEFAULT_MASS = 10
  private val DELTA_TIME = 0.001

  enum Command:
    case Start(iterations: Int, nBodies: Int, boundary: Boundary)
    case Stop
    case Resume
    case UpdatedBodyPosition(updatedPos: ActorBody.Message)

  enum Message:
    case Update(iteration: Int, positions: Seq[P2d])
    case Terminated

  def apply(coordinator: ActorRef[Message]): Behavior[Command] = Behaviors.setup { ctx =>
    new Simulation(coordinator).created
  }

case class Simulation(
    coordinator: ActorRef[Simulation.Message],
    maxIterations: Int = 10,
    bodyRefs: Set[ActorRef[ActorBody.Command]] = Set(),
    iteration: Int = 1
):
  import Simulation.Command
  import Simulation.Message
  import monocle.syntax.all._

  val created: Behavior[Command] = Behaviors.receivePartial { (ctx, msg) =>
    msg match
      case Command.Start(iterations, nBodies, boundary) =>
        val adapter = ctx.messageAdapter(Command.UpdatedBodyPosition.apply)
        val actorRefs =
          for
            i <- 0 until nBodies
            x = boundary.x0 * 0.25 + Random.nextDouble() * (boundary.x1 - boundary.x0) * 0.25
            y = boundary.y0 * 0.25 + Random.nextDouble() * (boundary.y1 - boundary.y0) * 0.25
            body = Body(id = i, pos = P2d(x, y), mass = DEFAULT_MASS)
            bodyRef = ctx.spawn(ActorBody(body, adapter, DELTA_TIME, boundary), s"body-$i")
          yield bodyRef
        for actorRef <- actorRefs do actorRef ! ActorBody.Start(actorRefs.toSet)
        this
          .focus(_.maxIterations)
          .replace(iterations)
          .focus(_.bodyRefs)
          .replace(actorRefs.toSet)
          .updating()
  }

  private def updating(positions: Seq[P2d] = Seq()): Behavior[Command] =
    Behaviors.receivePartial { (ctx, msg) =>
      msg match
        case Command.UpdatedBodyPosition(ActorBody.Message.UpdatedPos(pos)) =>
          val positionsUpdated = pos +: positions
          positionsAndIterationCheck(positionsUpdated)
        case Command.Stop => stop(positions)
    }

  private def positionsAndIterationCheck(positions: Seq[P2d]): Behavior[Command] =
    if positions.size == bodyRefs.size then
      coordinator ! Message.Update(iteration, positions)
      if iteration == maxIterations then coordinator ! Message.Terminated
      else for bodyRef <- bodyRefs do bodyRef ! ActorBody.PosUpdated()
      this.focus(_.iteration).modify(_ + 1).updating()
    else updating(positions)

  private def stop(snapshot: Seq[P2d]): Behavior[Command] = Behaviors.receivePartial { (ctx, msg) =>
    msg match
      case Command.UpdatedBodyPosition(ActorBody.Message.UpdatedPos(pos)) =>
        val snapshotUpdated = pos +: snapshot
        stop(snapshotUpdated)
      case Command.Resume =>
        positionsAndIterationCheck(snapshot)
  }
