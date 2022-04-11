ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.1"

libraryDependencies += "org.rogach" %% "scallop" % "4.1.0"

libraryDependencies += "org.apache.spark" %% "spark-core" % "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "ACO"
  )
