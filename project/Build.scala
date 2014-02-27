import sbt._
import sbt.Keys._

//import com.typesafe.sbt.pgp.PgpKeys._

//import sbtbuildinfo.Plugin._

object MyBuild extends Build {

  // Dependencies
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.3"
  lazy val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0"
  lazy val scalaMeter = "com.github.axel22" %% "scalameter" % "0.4"

  // Settings
  override lazy val settings = super.settings ++ Seq(
    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
      "-Yinline-warnings",
      "-deprecation",
      "-unchecked",
      "-optimize",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-feature"
    )
  )

  // Core
  lazy val core = Project("core", file(".")).
    settings(coreSettings: _*)

  lazy val coreSettings = Seq(
    name := "bignum",
    libraryDependencies ++= Seq(
      scalaCheck % "test"
    )
  )

  // Benchmark
  lazy val benchmarks = Project("benchmark", file("benchmark")).
    settings(bechmarkSettings: _*).
    dependsOn(core)

  lazy val bechmarkSettings = Seq(
    name := "bignum-benchmark",
    libraryDependencies ++= Seq(
      scalaMeter % "test"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    parallelExecution in Test := false
  )

}
