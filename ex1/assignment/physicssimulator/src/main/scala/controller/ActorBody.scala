package controller

import model.{Body, P2d}

object ActorBody:
  enum Command:
    case Start(body: Body)
  
  enum Message:
    case UpdatedPos(pos: P2d)
  
  enum BodyProtocol:
    case State(pos: P2d, mass: Double)
    case ForceUpdated
    case PosUpdated