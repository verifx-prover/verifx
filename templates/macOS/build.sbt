name := "crdt-proofs"
organization := "be.vub.kdeporre"
version := "0.2-SNAPSHOT"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "be.vub" %% "verifx" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
