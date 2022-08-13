package controller

import model.{Body, Boundary, P2d, V2d}
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import controller.ActorBody.Command
import controller.utils.Util

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

  private var currentResponses = 0
  private var repulsiveForce = V2d()

  val created: Behavior[Command] =
    Behaviors.withStash(10000) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case Start(bodyRefs) =>
            //ctx.log.info(s"BODY ACTOR - ${body.id}: received start")
            val refs = bodyRefs - ctx.self
            Util.sendToAll(refs)(State(body.pos, body.mass))
            stash.unstashAll(this.focus(_.actorBodies).replace(refs).force())
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

//  private def force(repulsiveForce: V2d = V2d(), currentResponses: Int = 0, iteration: Int = 1): Behavior[Command] =
//    Behaviors.withStash(actorBodies.size) { stash =>
//      Behaviors.receive { (ctx, msg) =>
//        msg match
//          case State(`iteration`, pos, mass) =>
//            //ctx.log.info(s"BODY ACTOR - ${body.id}: received STATE")
//            val currentResponsesUpdated = currentResponses + 1
//            val repulsiveForceUpdated = repulsiveForce + body.repulsiveForceBy(pos, mass).getOrElse(V2d())
//            if currentResponsesUpdated == actorBodies.size then
//              body = body.accelerate(repulsiveForceUpdated + body.currentFrictionForce)
//              Util.sendToAll(actorBodies, ForceUpdated(iteration))
//              ctx.log.info(s"BODY ACTOR - ${body.id}: it: $iteration force updated")
//              stash.unstashAll(waitForces(iteration = iteration))
//            else force(repulsiveForceUpdated, currentResponsesUpdated, iteration)
//          case other =>
//            stash.stash(other)
//            Behaviors.same
//      }
//    }

  private def force(): Behavior[Command] =
    Behaviors.withStash(actorBodies.size) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case State(pos, mass) =>
            //ctx.log.info(s"BODY ACTOR - ${body.id}: received STATE")
            currentResponses = currentResponses + 1
            repulsiveForce = repulsiveForce + body.repulsiveForceBy(pos, mass).getOrElse(V2d())
            //ctx.log.info(s"BODY ACTOR - ${body.id}: updating forces")
            if currentResponses == actorBodies.size then
              body = body.accelerate(repulsiveForce + body.currentFrictionForce)
              ctx.log.info(s"BODY ACTOR - ${body.id}: FORCE COMPUTED")
              Util.sendToAll(actorBodies)(ForceUpdated())
              currentResponses = 0
              repulsiveForce = V2d()
              stash.unstashAll(waitForces())
            else Behaviors.same
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

//  private def waitForces(iteration: Int, currentResponses: Int = 0): Behavior[Command] =
//    Behaviors.withStash(actorBodies.size) { stash =>
//      Behaviors.receive { (ctx, msg) =>
//        msg match
//          case ForceUpdated(`iteration`) =>
//            //ctx.log.info(s"BODY ACTOR - ${body.id}: force barrier")
//            val currentResponsesUpdated = currentResponses + 1
//            if currentResponsesUpdated == actorBodies.size then
//              body = body
//                .updateVelocity(dt)
//                .updatePos(dt)
//                .checkAndSolveBoundaryCollision(boundary)
//              simulation ! Message.UpdatedPos(iteration, body.pos)
//              Util.sendToAll(actorBodies, PosUpdated(iteration))
//              ctx.log.info(s"BODY ACTOR - ${body.id}: it: $iteration position updated")
//              stash.unstashAll(waitPos(iteration = iteration))
//            else waitForces(iteration, currentResponsesUpdated)
//          case other =>
//            stash.stash(other)
//            Behaviors.same
//      }
//    }

  private def waitForces(): Behavior[Command] =
    Behaviors.withStash(actorBodies.size) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case ForceUpdated() =>
            //ctx.log.info(s"BODY ACTOR - ${body.id}: force barrier")
            currentResponses = currentResponses + 1
            if currentResponses == actorBodies.size then
              body = body
                .updateVelocity(dt)
                .updatePos(dt)
                .checkAndSolveBoundaryCollision(boundary)
              ctx.log.info(s"BODY ACTOR - ${body.id}: POSITION UPDATED")
              simulation ! Message.UpdatedPos(body.pos)
              Util.sendToAll(actorBodies)(PosUpdated())
              currentResponses = 0
              stash.unstashAll(waitPos())
            else Behaviors.same
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

//  private def waitPos(iteration: Int, currentResponses: Int = 0): Behavior[Command] =
//    Behaviors.withStash(actorBodies.size) { stash =>
//      Behaviors.receive { (ctx, msg) =>
//        msg match
//          case PosUpdated(`iteration`) =>
//            //ctx.log.info(s"BODY ACTOR - ${body.id}: position barrier")
//            val currentResponsesUpdated = currentResponses + 1
//            if currentResponsesUpdated == actorBodies.size + 1 then // + 1 consider the simulation ack
//              val newIteration = iteration + 1
//              Util.sendToAll(actorBodies, State(newIteration, body.pos, body.mass))
//              ctx.log.info(s"BODY ACTOR - ${body.id}, it: $iteration: new iter")
//              stash.unstashAll(force(iteration = newIteration))
//            else waitPos(iteration, currentResponsesUpdated)
//          case other =>
//            stash.stash(other)
//            Behaviors.same
//      }
//    }

  private def waitPos(): Behavior[Command] =
    Behaviors.withStash(actorBodies.size) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case PosUpdated() =>
            //ctx.log.info(s"BODY ACTOR - ${body.id}: position barrier")
            currentResponses = currentResponses + 1
            if currentResponses == actorBodies.size + 1 then // + 1 consider the simulation ack
              ctx.log.info(s"BODY ACTOR - ${body.id}: new iter")
              Util.sendToAll(actorBodies)(State(body.pos, body.mass))
              currentResponses = 0
              stash.unstashAll(force())
            else Behaviors.same
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }
