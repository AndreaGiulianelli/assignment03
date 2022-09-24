package controller

import model.{Body, Boundary, P2d, V2d}
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import controller.utils.Util.*

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
      ActorBodyImpl(body, simulation, dt, boundary, actorBodies).created
    }

  private case class ActorBodyImpl(
      private var body: Body,
      simulation: ActorRef[ActorBody.Message],
      dt: Double,
      boundary: Boundary,
      actorBodies: Set[ActorRef[ActorBody.Command]]
  ):
    import ActorBody.*
    import monocle.syntax.all._

    val created: Behavior[Command] =
      Behaviors.withStash(10000) { stash =>
        Behaviors.receive { (ctx, msg) =>
          msg match
            case Start(bodyRefs) =>
              val refs = bodyRefs - ctx.self
              refs ! State(body.pos, body.mass)
              stash.unstashAll(this.focus(_.actorBodies).replace(refs).force())
            case other =>
              stash.stash(other)
              Behaviors.same
        }
      }

    def force(): Behavior[Command] =
      var currentResponses = 0
      var repulsiveForce = V2d()
      Behaviors.withStash(actorBodies.size) { stash =>
        Behaviors.receive { (ctx, msg) =>
          msg match
            case State(pos, mass) =>
              currentResponses = currentResponses + 1
              repulsiveForce = repulsiveForce + body.repulsiveForceBy(pos, mass).getOrElse(V2d())
              if currentResponses == actorBodies.size then
                body = body.accelerate(repulsiveForce + body.currentFrictionForce)
                actorBodies ! ForceUpdated()
                stash.unstashAll(waitForces())
              else Behaviors.same
            case other =>
              stash.stash(other)
              Behaviors.same
        }
      }

    def waitForces(): Behavior[Command] =
      var currentResponses = 0
      Behaviors.withStash(actorBodies.size) { stash =>
        Behaviors.receive { (ctx, msg) =>
          msg match
            case ForceUpdated() =>
              currentResponses = currentResponses + 1
              if currentResponses == actorBodies.size then
                body = body
                  .updateVelocity(dt)
                  .updatePos(dt)
                  .checkAndSolveBoundaryCollision(boundary)
                //ctx.log.info(s"BODY ACTOR - ${body.id}: POSITION UPDATED")
                simulation ! Message.UpdatedPos(body.pos)
                actorBodies ! PosUpdated()
                stash.unstashAll(waitPos())
              else Behaviors.same
            case other =>
              stash.stash(other)
              Behaviors.same
        }
      }

    def waitPos(): Behavior[Command] =
      var currentResponses = 0
      Behaviors.withStash(actorBodies.size) { stash =>
        Behaviors.receive { (ctx, msg) =>
          msg match
            case PosUpdated() =>
              currentResponses = currentResponses + 1
              if currentResponses == actorBodies.size + 1 then // + 1 consider the simulation ack
                //ctx.log.info(s"BODY ACTOR - ${body.id}: new iter")
                actorBodies ! State(body.pos, body.mass)
                stash.unstashAll(force())
              else Behaviors.same
            case other =>
              stash.stash(other)
              Behaviors.same
        }
      }
