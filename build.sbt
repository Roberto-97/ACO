version := "0.1.0"

scalaVersion := "2.11.12"

libraryDependencies += "org.rogach" %% "scallop" % "3.4.0"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.6.2"

lazy val root = (project in file("."))
  .settings(
    name := "ACO"
  ).enablePlugins(AssemblyPlugin)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}