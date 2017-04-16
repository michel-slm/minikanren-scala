enablePlugins(ScalaJSPlugin)

name := "Scala MiniKanren root project"
crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1")
scalaVersion in ThisBuild := "2.12.1" // or any other Scala version >= 2.10.2 for Scala.js

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

lazy val root = project.in(file(".")).
  aggregate(miniKanrenJS, miniKanrenJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val miniKanren = crossProject.in(file(".")).
  settings(
    name := "Scala MiniKanren",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test",

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-unchecked",
      "-feature",
      //"-language:implicitConversions",
      //"-language:postfixOps",
      //"-language:higherKinds",
      //"-language:reflectiveCalls",
      "-Xlint",
      //  "-Xfatal-warnings",
      //"-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture"
    )

  ).jvmSettings(
    coverageEnabled := true,
    initialCommands := """
                         |import info.hircus.kanren.MiniKanren._
                         |import info.hircus.kanren.Prelude._
                         |import info.hircus.kanren.MKMath._
                         |import info.hircus.kanren.examples.PalProd._
                         |import info.hircus.kanren.examples.SendMoreMoney._
                         |
                         |var x = make_var('x)
                         |var y = make_var('y)
                         |var z = make_var('z)
                         |
                         |def time(block: => Any) = {
                         |  val start = System currentTimeMillis ()
                         |  val res   = block
                         |  val stop  = System currentTimeMillis ()
                         |  ((stop-start), res)
                         |}
                         |
                         |def ntimes(n: Int, block: => Any) = {
                         |  // folding a list of Longs is cumbersome
                         |  def adder(x:Long,y:Long) = x+y
                         |  val zero : Long = 0
                         |
                         |  // compute only once!
                         |  val res = (for (i <- 0 until n) yield (time(block) _1)).toList
                         |  println("Elapsed times: " + res)
                         |  println("Avg: " + (res.foldLeft(zero)(adder) / n))
                         |}
                         |""".stripMargin,
  tutSettings
  ).jsSettings(
    coverageEnabled := false
  )

lazy val miniKanrenJVM = miniKanren.jvm

lazy val miniKanrenJS = miniKanren.js

//tutSettings

//tutSourceDirectory := baseDirectory.value / "shared" / "src" / "main" / "tut"

LaikaPlugin.defaults

inConfig(LaikaKeys.Laika)(Seq(
//  sourceDirectories := Seq(baseDirectory.value / "docs"),
  LaikaKeys.encoding := "UTF-8"
))