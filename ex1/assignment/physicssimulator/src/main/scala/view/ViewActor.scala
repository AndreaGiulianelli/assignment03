package view

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import model.{Boundary, P2d}
import view.gui.SimulationViewer
import controller.Coordinator

object ViewActor:
  enum Command:
    case Start(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command])
    case Update(positions: Seq[P2d], vt: Double, iter: Int, boundary: Boundary)
    case Terminated

  def apply(): Behavior[Command] = init()

  private def init(): Behavior[Command] =
    Behaviors.receivePartial { case (ctx, Command.Start(w, h, coordinator)) =>
      val viewer = SimulationViewer(w, h, coordinator)
      viewer.display()
      initialized(viewer)
    }

  private def initialized(viewer: SimulationViewer): Behavior[Command] = Behaviors.receivePartial { (ctx, msg) =>
    msg match
      case Command.Update(positions, vt, iter, boundary) =>
        ctx.log.info("VIEW ACTOR: received update")
        viewer.update(positions, vt, iter, boundary)
        Behaviors.same
      case Command.Terminated =>
        ctx.log.info("VIEW ACTOR: received TERMINATION")
        viewer.simulationEnd()
        Behaviors.stopped
  }
