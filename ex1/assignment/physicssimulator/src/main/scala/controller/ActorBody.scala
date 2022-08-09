package controller

import model.{Body, Boundary, P2d, V2d}
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object ActorBody:
  trait Command
  case class Start(actorBodies: Set[ActorRef[Command]]) extends Command
  case class PosUpdated() extends Command
  private case class State(pos: P2d, mass: Double) extends Command
  private case class ForceUpdated() extends Command

  enum Message:
    case UpdatedPos(pos: P2d)

  def apply(
      body: Body,
      simulation: ActorRef[Message],
      dt: Double,
      boundary: Boundary,
      actorBodies: Set[ActorRef[Command]] = Set()
  ): Behavior[Command] =
    Behaviors.setup { ctx =>
      new ActorBody(body, simulation, dt, boundary, actorBodies).created
    }

case class ActorBody(
    private var body: Body,
    simulation: ActorRef[ActorBody.Message],
    dt: Double,
    boundary: Boundary,
    actorBodies: Set[ActorRef[ActorBody.Command]]
):
  import ActorBody.*
  import monocle.syntax.all._

  val created: Behavior[Command] =
    Behaviors.withStash(100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case Start(bodyRefs) =>
            sendToAll(State(body.pos, body.mass))
            stash.unstashAll(this.focus(_.actorBodies).replace(bodyRefs).force())
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

  private def force(repulsiveForce: V2d = V2d(), currentResponses: Int = 0): Behavior[Command] =
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
            val currentResponsesUpdated = currentResponses + 1
            if currentResponsesUpdated == actorBodies.size then
              body = body
                .updateVelocity(dt)
                .updatePos(dt)
                .checkAndSolveBoundaryCollision(boundary)
              simulation ! Message.UpdatedPos(body.pos)
              sendToAll(PosUpdated())
              stash.unstashAll(waitPos())
            else waitForces(currentResponsesUpdated)
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
            val currentResponsesUpdated = currentResponses + 1
            if currentResponsesUpdated == actorBodies.size + 1 then // + 1 consider the simulation ack
              sendToAll(State(body.pos, body.mass))
              stash.unstashAll(force())
            else waitPos(currentResponsesUpdated)
          case other =>
            stash.stash(other)
            Behaviors.same

      }
    }

  private def sendToAll(msg: Command): Unit =
    for actorRef <- actorBodies do actorRef ! msg
