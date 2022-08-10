package controller.utils

import akka.actor.typed.ActorRef

object Util:
  def sendToAll[A](refs: Set[ActorRef[A]], msg: A): Unit =
    for actorRef <- refs do actorRef ! msg
