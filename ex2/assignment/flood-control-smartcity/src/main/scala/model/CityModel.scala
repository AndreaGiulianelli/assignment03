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

  enum ZoneStatus:
    case NORMAL
    case ALARM
    case UNDER_MANAGEMENT
  case class Zone(zoneId: Int, sensors: Int = 0, status: ZoneStatus = ZoneStatus.NORMAL)

  case class PluviometerSensor(
      pluviometerId: Int,
      associatedZone: Zone,
      senseRate: Int,
      alarmThreshold: Double = Random.between(0.5, 1)
  ):
    def inAlarm: Boolean =
      val r = Random.between(0.0, 1.0)
      r >= alarmThreshold

  enum FirestationStatus:
    case FREE
    case BUSY
  case class FirestationService(associatedZone: Zone, status: FirestationStatus = FirestationStatus.FREE)

  case class City(
      zones: Seq[Zone],
      firestations: Seq[FirestationService],
      sensors: Seq[PluviometerSensor]
  )

  given Conversion[Int, Zone] with
    override def apply(id: Int): Zone = Zone(id)

  def generateCity()(using context: SmartCityParameters): City =
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

  private def generateSensors(zones: Seq[Zone], sensorPerZone: Int, senseRate: Int): Seq[PluviometerSensor] =
    for
      zone <- zones
      s <- 0 until sensorPerZone
    yield PluviometerSensor(zone.zoneId * sensorPerZone + s, zone, senseRate)
