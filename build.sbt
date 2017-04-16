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
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test"
  ).jvmSettings(
    coverageEnabled := true
  ).jsSettings(
    coverageEnabled := false
  )

lazy val miniKanrenJVM = miniKanren.jvm

lazy val miniKanrenJS = miniKanren.js

tutSettings