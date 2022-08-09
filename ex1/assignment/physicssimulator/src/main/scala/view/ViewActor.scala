package view

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import model.{Boundary, P2d}
import view.gui.SimulationViewer
import controller.Coordinator

object ViewActor:
  enum Command:
    case Start(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command])
    case Update(positions: Seq[P2d], vt: Double, iter: Long, boundary: Boundary)
    case Terminated

  def apply(): Behavior[Command] = init()

  // todo: check, i think that the stashing here is not necessary considering that the view actor is the first created and the only one that the guardian send a message. It starts all from here
  private def init(): Behavior[Command] =
    Behaviors.withStash[Command](100) { stash =>
      Behaviors.receive { (ctx, msg) =>
        msg match
          case Command.Start(w, h, coordinator) =>
            val viewer = SimulationViewer(w, h, coordinator)
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
