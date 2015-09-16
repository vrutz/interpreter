import sbt._
import Keys._

object build extends Build {
  lazy val ScalaVersion = "2.11.7"
  lazy val LibraryVersion = "0.1.0-SNAPSHOT"

  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    test in Test := (test in tests in Test).value
  ) aggregate (interpreter, tests)

  lazy val interpreter = Project(
    id   = "interpreter",
    base = file("interpreter")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies += "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT"
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
  ) dependsOn (interpreter)

  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := ScalaVersion,
    crossVersion := CrossVersion.binary,
    version := LibraryVersion,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false
  )
}