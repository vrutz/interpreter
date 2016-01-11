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
    run in Compile := (run in interpreter in Compile).evaluated,
    test in Test := (test in tests in Test).value
  ) aggregate (interpreter, tests)

  // lazy val scalameta = Project(
  //   id = "scalameta"
  //   base = file("scalameta")
  // ) settings (
  //   sharedSettings: _*
  // ) settings (
  //   addCompilerPlugin("org.scalameta" % "scalahost" % "0.1.0-SNAPSHOT" cross CrossVersion.full),
  //   scalacOptions += "-Ybackend:GenBCode"
  // )

  lazy val scrutinee = Project(
    id = "scrutinee",
    base = file("scrutinee")
  ) settings (
    sharedSettings: _*
  ) settings (
    addCompilerPlugin("org.scalameta" % "scalahost" % "0.1.0-SNAPSHOT" cross CrossVersion.full),
    scalacOptions += "-Ybackend:GenBCode"
  )

  lazy val interpreter = Project(
    id   = "interpreter",
    base = file("interpreter")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies += "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalameta" %% "scalahost" % "0.1.0-SNAPSHOT" cross CrossVersion.full,
    (fork in run) := true,
    (javaOptions in run) ++= {
        val sbt_classpath = (fullClasspath in scrutinee in Compile).value
        val classpath = sbt_classpath.map(_.data.getAbsolutePath).mkString(java.io.File.pathSeparator)
        val sourcepath = (sourceDirectory in scrutinee in Compile).value.getAbsolutePath
        Seq(s"-Dsbt.paths.scrutinee.classes=$classpath", s"-Dsbt.paths.scrutinee.sources=$sourcepath")
      }
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies += "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalameta" %% "scalahost" % "0.1.0-SNAPSHOT" cross CrossVersion.full,
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    sourceDirectory in Test := {
      val defaultValue = (sourceDirectory in Test).value
      System.setProperty("sbt.paths.scrutinee.sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    fullClasspath in Test := {
      val defaultValue = (fullClasspath in Test).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      val scalaLibrary = classpath.map(_.toString).find(_.contains("scala-library")).get
      // val scalameta = classpath.map(_.toString).find(_.contains("scalameta")).get
      System.setProperty("sbt.paths.scalalibrary.classes", scalaLibrary)
      // System.setProperty("sbt.paths.scalameta.classes", scalameta)
      System.setProperty("sbt.paths.scrutinee.classes", classpath.mkString(java.io.File.pathSeparator))
      defaultValue
    }
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
