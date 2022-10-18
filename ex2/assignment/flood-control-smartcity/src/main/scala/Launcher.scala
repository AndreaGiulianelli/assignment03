import model.CityModel.{SmartCityParameters, generateCity}
import util.Utils.{deployFireStation, deployPluviometer, deployZone}

object Launcher:
  @main def main(): Unit =
    given SmartCityParameters with
      override val zones: Int = 5
      override val sensorPerZone: Int = 4
      override val senseRate: Int = 1000
    val city = generateCity()
    city.zones.foreach(deployZone)
    city.firestations.foreach(deployFireStation)
    city.sensors.foreach(deployPluviometer)
