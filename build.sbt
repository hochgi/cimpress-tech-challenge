name := "cimpress-tech-challenge"

version := "0.1"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "io.spray" %% "spray-can" % "1.3.3",
  "io.spray" %% "spray-json" % "1.3.2",
  "io.spray" %% "spray-httpx" % "1.3.3",
  "com.typesafe.akka" %% "akka-actor" % "2.3.10",
  "org.rogach" %% "scallop" % "0.9.5"
)