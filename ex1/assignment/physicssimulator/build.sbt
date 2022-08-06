ThisBuild / scalaVersion := "3.1.1"

lazy val akkaVersion = "2.6.19"
lazy val akkaGroup = "com.typesafe.akka"
lazy val root = (project in file("."))
  .settings(
    name := "physicssimulator",
    libraryDependencies ++= Seq(
      akkaGroup %% "akka-actor-typed" % akkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "dev.optics" %% "monocle-core" % "3.1.0",
      "dev.optics" %% "monocle-macro" % "3.1.0"
    )
  )
