import model.CityModel.{FirestationService, PluviometerSensor, SmartCityParameters, Zone, generateCity}
import pluviometer.Pluviometer
import firestation.Firestation
import util.Utils.{deployFireStation, deployPluviometer, deployZone, startNode}
import zone.ZoneControl

object Launcher:
  import model.CityModel.given

  @main def main(): Unit =
    given SmartCityParameters with
      override val zones: Int = 2
      override val sensorPerZone: Int = 1
      override val senseRate: Int = 1000
    val city = generateCity()
    city.zones.foreach(deployZone)
    city.firestations.foreach(deployFireStation)
    city.sensors.foreach(deployPluviometer)
