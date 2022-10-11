import model.CityModel.{FirestationService, PluviometerSensor}
import util.Utils.{deployFireStation, deployPluviometer, deployZone}

object SingleLauncher:
  // Zones
  @main def launchZone0(): Unit =
    deployZone(0)

  @main def launchZone1(): Unit =
    deployZone(1)

  @main def launchZone2(): Unit =
    deployZone(2)

  // FireStation
  @main def launchFirestation0(): Unit =
    deployFireStation(FirestationService(0))

  @main def launchFirestation1(): Unit =
    deployFireStation(FirestationService(1))

  @main def launchFirestation2(): Unit =
    deployFireStation(FirestationService(2))

  //Pluviometer
  private val senseRate = 1000

  @main def launchPluviometer0(): Unit =
    deployPluviometer(PluviometerSensor(0, 0, senseRate))

  @main def launchPluviometer1(): Unit =
    deployPluviometer(PluviometerSensor(1, 0, senseRate))

  @main def launchPluviometer2(): Unit =
    deployPluviometer(PluviometerSensor(2, 0, senseRate))

  @main def launchPluviometer3(): Unit =
    deployPluviometer(PluviometerSensor(3, 1, senseRate))

  @main def launchPluviometer4(): Unit =
    deployPluviometer(PluviometerSensor(4, 1, senseRate))

  @main def launchPluviometer5(): Unit =
    deployPluviometer(PluviometerSensor(5, 1, senseRate))

  @main def launchPluviometer6(): Unit =
    deployPluviometer(PluviometerSensor(6, 2, senseRate))

  @main def launchPluviometer7(): Unit =
    deployPluviometer(PluviometerSensor(7, 2, senseRate))

  @main def launchPluviometer8(): Unit =
    deployPluviometer(PluviometerSensor(8, 2, senseRate))
