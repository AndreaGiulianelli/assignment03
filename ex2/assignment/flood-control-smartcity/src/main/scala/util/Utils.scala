package util

import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory

object Utils:
  def startNode[X](configFile: String = "cluster-config")(role: String, port: Int)(
      root: => Behavior[X]
  ): ActorSystem[X] =
    val config = ConfigFactory
      .parseString(s"""
        akka.remote.artery.canonical.port=$port
        akka.cluster.roles = [$role]
        """)
      .withFallback(ConfigFactory.load(configFile))

    ActorSystem(root, "FCSNode", config)
