name := "verifx"
organization := "be.vub"
version := "1.0.0"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalameta" %% "scalameta" % "4.3.20",
  "commons-codec" % "commons-codec" % "1.13",
  "org.jgrapht" % "jgrapht-core" % "1.5.1",
  "org.scala-graph" %% "graph-core" % "1.13.0"
)

assemblyJarName in assembly := "verifx.jar"
target in assembly := file("out/")
test in assembly := {} // don't run tests
mainClass in assembly := Some("be.vub.verifx.ProofRunner")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case PathList("reference.conf") => MergeStrategy.concat
  case _ => MergeStrategy.first
}
