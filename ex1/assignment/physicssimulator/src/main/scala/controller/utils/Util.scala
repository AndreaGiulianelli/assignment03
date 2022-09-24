package controller.utils

import akka.actor.typed.ActorRef

import scala.annotation.targetName

object Util:
  extension [A](refs: Set[ActorRef[A]])
    @targetName("sendAll")
    def !(msg: A): Unit =
      for actorRef <- refs do actorRef ! msg
