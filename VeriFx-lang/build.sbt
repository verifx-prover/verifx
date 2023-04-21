name := "verifx"
version := "1.0.1"

organization := "org.verifx"
developers := List(Developer("Kevin", "De Porre", "kevin.de.porre@vub.be", url("https://github.com/kevin-dp")))
scmInfo := Some(ScmInfo(url("https://github.com/kevin-dp/verifx"), "git@github.com:kevin-dp/verifx.git"))
homepage := Some(url("https://github.com/kevin-dp/verifx"))
licenses += ("MIT", url("http://www.opensource.org/licenses/mit-license.php"))
publishMavenStyle := true

scalaVersion := "2.13.1"

// To release a new version,
// increment the VeriFx version on line 2,
// and execute `publishSigned` in an SBT shell.
// Then log in to https://s01.oss.sonatype.org/
// you should see VeriFx under "staging repositories"
// verify that it contains the necessary files,
// then close it, and if it successfully closed, then release it.
// Within ~30 minutes it should become available
// and within ~2 hours it should be listed in search results in maven central.

// Remove all additional repository other than Maven Central from POM
pomIncludeRepository := { _ => false }
publishTo := {
  // For accounts created after Feb 2021:
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

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
