import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import controller.Coordinator
import model.Boundary
import view.ViewActor

object LauncherNoGUI extends App:
  def main(): Unit =
    ActorSystem(
      Behaviors.setup { ctx =>
        val iterations = 50
        val bodyNumber = 500
        val boundary = Boundary(-6, -6, 6, 6)
        val coordinator = ctx.spawn(Coordinator(None, iterations, bodyNumber, boundary), "coordinator")
        coordinator ! Coordinator.Command.Start
        Behaviors.empty
      },
      "physics-simulator"
    )
  main()
