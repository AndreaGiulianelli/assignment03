package controller.utils

import akka.actor.typed.ActorRef

object Util:
  extension [A](refs: Set[ActorRef[A]])
    def sendToAll(msg: A): Unit =
      for actorRef <- refs do actorRef ! msg
