import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import controller.Coordinator
import model.Boundary
import view.ViewActor

object Launcher:
  @main def main(): Unit =
    ActorSystem(
      Behaviors.setup { ctx =>
        val iterations = 50000
        val bodyNumber = 500
        val boundary = Boundary(-6, -6, 6, 6)
        val viewActor = ctx.spawn(ViewActor(), "view")
        val coordinator = ctx.spawn(Coordinator(Some(viewActor), iterations, bodyNumber, boundary), "coordinator")
        viewActor ! ViewActor.Command.Start(620, 620, coordinator)
        Behaviors.empty
      },
      "physics-simulator"
    )
