package controller

import model.{Body, Boundary, P2d, V2d}
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object ActorBody:
  trait Command
  case class Start() extends Command
  case class PosUpdated() extends Command
  private case class State(pos: P2d, mass: Double) extends Command
  private case class ForceUpdated() extends Command

  enum Message:
    case UpdatedPos(pos: P2d)

  def apply(body: Body, simulation: ActorRef[Message], actorBodies: Set[ActorRef[Command]]): Behavior[Command] =
    Behaviors.setup { ctx =>
      ???
    }

class ActorBody(
    private var body: Body,
    simulation: ActorRef[ActorBody.Message],
    actorBodies: Set[ActorRef[ActorBody.Command]],
    dt: Double,
    boundary: Boundary
):
  import ActorBody.*
  import monocle.syntax.all._

  val created: Behavior[Command] =
    Behaviors.withStash(100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case Start() =>
            sendToAll(State(body.pos, body.mass))
            stash.unstashAll(force())
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

  private def force(repulsiveForce: V2d = V2d(0, 0), currentResponses: Int = 0): Behavior[Command] =
    Behaviors.withStash(100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case State(pos, mass) =>
            val currentResponsesUpdated = currentResponses + 1
            val repulsiveForceUpdated = repulsiveForce + body.repulsiveForceBy(pos, mass).get
            if currentResponsesUpdated == actorBodies.size then
              body = body.accelerate(repulsiveForceUpdated + body.currentFrictionForce)
              sendToAll(ForceUpdated())
              stash.unstashAll(waitForces())
            else force(repulsiveForceUpdated, currentResponsesUpdated)
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

  private def waitForces(currentResponses: Int = 0): Behavior[Command] =
    Behaviors.withStash(100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case ForceUpdated() =>
            val currentResponseUpdated = currentResponses + 1
            if currentResponseUpdated == actorBodies.size then
              body = body
                .updateVelocity(dt)
                .updatePos(dt)
                .checkAndSolveBoundaryCollision(boundary)
              simulation ! Message.UpdatedPos(body.pos)
              sendToAll(PosUpdated())
              stash.unstashAll(waitPos())
            else waitForces(currentResponses)
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

  private def waitPos(currentResponses: Int = 0): Behavior[Command] =
    Behaviors.withStash(100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case PosUpdated() =>
            val currentResponseUpdated = currentResponses + 1
            if currentResponseUpdated == actorBodies.size + 1 then // + 1 consider the simulation ack
              stash.unstashAll(force())
            else waitPos(currentResponseUpdated)
          case other =>
            stash.stash(other)
            Behaviors.same

      }
    }

  private def sendToAll(msg: Command): Unit =
    for actorRef <- actorBodies do actorRef ! msg
