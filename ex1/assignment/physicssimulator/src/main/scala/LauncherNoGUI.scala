import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import controller.Coordinator
import model.Boundary

object LauncherNoGUI extends App:
  def main(): Unit =
    ActorSystem(
      Behaviors.setup { ctx =>
        val iterations = 1000
        val bodyNumber = 100
        val boundary = Boundary(-6, -6, 6, 6)
        val coordinator = ctx.spawn(Coordinator(None, iterations, bodyNumber, boundary), "coordinator")
        coordinator ! Coordinator.Command.Start
        Behaviors.empty
      },
      "physics-simulator"
    )
  main()
