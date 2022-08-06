package controller

object Coordinator:
  enum Command:
    case Start
    case Stop
    case Resume
    case UpdatedPositions(updatedPositions: Simulation.Message.Update)
    case Terminated
