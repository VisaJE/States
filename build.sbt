

ThisBuild / organization := "org.eemil"
ThisBuild / scalaVersion := "2.12.6"
ThisBuild / version := "0.1.0"
trapExit := false

lazy val root = (project in file("."))
  .settings(
    // set the name of the project
    name := "States",

    // set the main Scala source directory to be <base>/src
    scalaSource in Compile := baseDirectory.value / "src",

    mainClass in (Compile, run) := Some("states.Käyttöliittymä")
    )

