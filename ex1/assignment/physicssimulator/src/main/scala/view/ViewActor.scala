package view

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import model.{Boundary, P2d}
import view.gui.SimulationViewer
import controller.Coordinator

/** Module that model the ViewActor actor. */
object ViewActor:
  enum Command:
    case Start(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command])
    case Update(positions: Seq[P2d], vt: Double, iter: Int, boundary: Boundary)
    case Terminated

  def apply(): Behavior[Command] = init()

  private def init(): Behavior[Command] =
    Behaviors.receivePartial { case (_, Command.Start(w, h, coordinator)) =>
      val viewer = SimulationViewer(w, h, coordinator)
      viewer.display()
      initialized(viewer)
    }

  private def initialized(viewer: SimulationViewer): Behavior[Command] = Behaviors.receivePartial { (_, msg) =>
    msg match
      case Command.Update(positions, vt, iter, boundary) =>
        viewer.update(positions, vt, iter, boundary)
        Behaviors.same
      case Command.Terminated =>
        viewer.simulationEnd()
        Behaviors.stopped
  }
