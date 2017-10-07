import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "systems.flowing",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Cake examples",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value//,
    //libraryDependencies += "systems.flowing" %% "cake" % "0.1.0-SNAPSHOT" from "http://cake.flowing.systems/latest"
  )
