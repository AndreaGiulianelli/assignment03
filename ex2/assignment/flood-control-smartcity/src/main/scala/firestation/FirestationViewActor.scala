package firestation

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import firestation.gui.FirestationViewer
import firestation.Firestation
import model.CityModel.{FirestationService, Zone}

object FirestationViewActor:
  enum Command:
    case Start(zone: Zone, width: Int, height: Int, firestation: ActorRef[Firestation.Command])
    case UpdateFirestation(firestation: FirestationService)

  def apply(): Behavior[Command] = init()

  private def init(): Behavior[Command] =
    Behaviors.receiveMessagePartial { case Command.Start(zone, width, height, firestation) =>
      val viewer = FirestationViewer(zone, width, height, firestation)
      viewer.display()
      initialized(viewer)
    }

  private def initialized(viewer: FirestationViewer): Behavior[Command] = Behaviors.receiveMessagePartial {
    case Command.UpdateFirestation(firestation) =>
      viewer.update(firestation)
      Behaviors.same
  }
