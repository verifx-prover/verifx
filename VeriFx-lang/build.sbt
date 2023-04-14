name := "verifx"
version := "1.0.0"

organization := "org.verifx"
developers := List(Developer("Kevin", "De Porre", "kevin.de.porre@vub.be", url("https://github.com/kevin-dp")))
scmInfo := Some(ScmInfo(url("https://github.com/kevin-dp/verifx"), "git@github.com:kevin-dp/verifx.git"))
homepage := Some(url("https://github.com/kevin-dp/verifx"))
licenses += ("MIT", url("http://www.opensource.org/licenses/mit-license.php"))
publishMavenStyle := true

scalaVersion := "2.13.1"

// disable publish with scala version,
// otherwise artifact name will include scala version
// e.g verifx_2.13.1
crossPaths := false

// add sonatype repository settings
// snapshot versions publish to sonatype snapshot repository
// other versions publish to sonatype staging repository
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

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
mainClass in assembly := Some("org.verifx.ProofRunner")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case PathList("reference.conf") => MergeStrategy.concat
  case _ => MergeStrategy.first
}
