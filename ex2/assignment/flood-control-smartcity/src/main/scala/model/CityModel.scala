package model

import scala.util.Random

object CityModel:
  type Point2D = (Int, Int)

  trait SmartCityParameters:
    /** The number of zones in the city
      * @return
      *   the number of zones
      */
    def zones: Int
    /** The number of sensor for each zone
      * @return
      *   the number of sensors for each zones
      */
    def sensorPerZone: Int
    /** The sense rate of the sensors
      * @return
      *   the sense rate in milliseconds
      */
    def senseRate: Int

  trait Sensor:
    def associatedZone: Zone
    def alarmThreshold: Double

  case class Zone(zoneId: Int)
  case class PluviometerSensor(pluviometerId: String, associatedZone: Zone, alarmThreshold: Double, senseRate: Int)
      extends Sensor
  case class FirestationService(associatedZone: Zone)
  case class City(
      zones: Seq[Zone],
      firestations: Seq[FirestationService],
      sensors: Seq[Sensor]
  )

  def generateCity(using context: SmartCityParameters): City =
    val zones = generateZones(context.zones)
    val firestations = generateStations(zones)
    val sensors = generateSensors(zones, context.sensorPerZone, context.senseRate)
    City(zones, firestations, sensors)

  private def generateZones(zones: Int): Seq[Zone] =
    for zoneId <- 0 until zones
    yield Zone(zoneId)

  private def generateStations(zones: Seq[Zone]): Seq[FirestationService] =
    for zone <- zones
    yield FirestationService(zone)

  private def generateSensors(zones: Seq[Zone], sensorPerZone: Int, senseRate: Int): Seq[Sensor] =
    for
      zone <- zones
      s <- 0 until sensorPerZone
      st = Random.between(0.5, 1)
    yield PluviometerSensor(s"$zone-$s", zone, st, senseRate)
