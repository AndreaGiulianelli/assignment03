package util

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory
import firestation.Firestation
import model.CityModel.{FirestationService, PluviometerSensor, Zone}
import pluviometer.Pluviometer
import zone.ZoneControl

import scala.annotation.targetName

object Utils:
  def deployZone(zone: Zone): Unit =
    startNode("zone", 2551 + zone.zoneId)(ZoneControl(zone))

  def deployFireStation(firestation: FirestationService): Unit =
    startNode("firestation", 7000 + firestation.associatedZone.zoneId)(Firestation(firestation))

  def deployPluviometer(pluviometer: PluviometerSensor): Unit =
    startNode("sensor", 9000 + pluviometer.pluviometerId)(Pluviometer(pluviometer))

  private def startNode[X](role: String, port: Int, configFile: String = "cluster-config")(
      root: => Behavior[X]
  ): ActorSystem[X] =
    val config = ConfigFactory
      .parseString(s"""
        akka.remote.artery.canonical.port=$port
        akka.cluster.roles = [$role]
        """)
      .withFallback(ConfigFactory.load(configFile))

    ActorSystem(root, "FCSNode", config)

  object AkkaUtils:
    extension [A](refs: Set[ActorRef[A]])
      @targetName("sendAll")
      def !(msg: A): Unit =
        for actorRef <- refs do actorRef ! msg
