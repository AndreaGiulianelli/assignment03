package controller

import model.P2d

object Simulation:
  enum Command:
    case Start(iterations: Int)
    case Stop
    case Resume

  enum Message:
    case Update(positions: Seq[P2d])
    case Terminated
    
  enum BodyProtocol:
    case PosUpdated
    case UpdatedBodyPosition(updatedPos: ActorBody.Message.UpdatedPos)
