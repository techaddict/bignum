import sbt._
import sbt.Keys._

//import com.typesafe.sbt.pgp.PgpKeys._

//import sbtbuildinfo.Plugin._

object MyBuild extends Build {

  // Dependencies
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.6"

  // Settings
  override lazy val settings = super.settings ++ Seq(
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.10.4", "2.11.4"),
    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
      //"-Yinline-warnings",
      "-deprecation",
      "-feature",
      "-unchecked"
      //"-optimize",
      //"-language:experimental.macros",
      //"-language:higherKinds",
      //"-language:implicitConversions",
      //"-feature"
    )
  )

  // Core
  lazy val core = Project("core", file(".")).
    settings(coreSettings: _*)

  lazy val coreSettings = Seq(
    name := "bignum",
    libraryDependencies ++= Seq(
      scalaCheck % "test"
    ),
    initialCommands in console := """
    import bignum.BigInt2
    import bignum.BigInt2._
    """
  )

  // Benchmark
  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(core)

  lazy val key = AttributeKey[Boolean]("javaOptionsPatched")

  lazy val benchmarkSettings = Seq(
    name := "bignum-benchmark",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.1",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "2.2.4"
    ),
    parallelExecution := false,
    fork in run := true,
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(key) match {
          case None =>
            val classPath = Project.runTask(fullClasspath in Runtime in benchmark, state).get._2.toEither.right.get.files.mkString(":")
            Project.extract(state).append(Seq(javaOptions in (benchmark, run) ++= Seq("-cp", classPath)), state.put(key, true))
          case Some(_) =>
            state
        }
      }
    }
  )

}
