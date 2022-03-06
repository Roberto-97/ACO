ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

libraryDependencies += "org.rogach" %% "scallop" % "4.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "ACO"
  )
