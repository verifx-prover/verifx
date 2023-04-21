addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.4")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

// sbt-sonatype plugin used to publish artifact to maven central via sonatype nexus
// sbt-pgp plugin used to sign the artifcat with pgp keys
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")
