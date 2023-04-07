name := "linux-example"
organization := "be.vub"
version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "be.vub" %% "verifx" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
