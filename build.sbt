name := """assessments"""

version := "1.0-SNAPSHOT"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.6.3"

//lazy val root = project in file(".")

lazy val webapp = (project in file("webapp"))
  .settings(
    libraryDependencies += guice,
    libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
//    libraryDependencies += "org.commonmark" % "commonmark" % "0.22.0", // TODO remove
  )
  .enablePlugins(PlayScala)
  .dependsOn(problems)

lazy val problems = (project in file("problems"))
  .settings(
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "org.commonmark" % "commonmark" % "0.22.0",
//    libraryDependencies += "com.github.benoitlouy" %% "indent" % "0.8.0",
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.4",
    libraryDependencies += "dev.scalapy" %% "scalapy-core" % "0.5.3",
    libraryDependencies += "com.eed3si9n.eval" % "eval" % "0.3.0" cross CrossVersion.full,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0",
  )
