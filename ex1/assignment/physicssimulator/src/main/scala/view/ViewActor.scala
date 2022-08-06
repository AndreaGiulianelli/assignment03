package view

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import model.{Boundary, P2d}
import view.gui.SimulationViewer

object ViewActor:
  enum Command:
    case Start(w: Int, h: Int)
    case Update(positions: Seq[P2d], vt: Double, iter: Long, boundary: Boundary)
    case Terminated

  def apply(): Behavior[Command] = init()

  private def init(): Behavior[Command] =
    Behaviors.withStash[Command](100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case Command.Start(w, h) =>
            val viewer = SimulationViewer(w, h)
            viewer.display()
            stash.unstashAll(initialized(viewer))
          case other =>
            stash.stash(other)
            Behaviors.same
      }
    }

  private def initialized(viewer: SimulationViewer): Behavior[Command] = Behaviors.receivePartial { (ctx, msg) =>
    msg match
      case Command.Update(positions, vt, iter, boundary) =>
        viewer.update(positions, vt, iter, boundary)
        Behaviors.same
      case Command.Terminated =>
        viewer.simulationEnd()
        Behaviors.stopped
  }
