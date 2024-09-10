name := """assessments"""

version := "1.0-SNAPSHOT"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.3.3"

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
    libraryDependencies += "com.github.benoitlouy" %% "indent" % "0.8.0",
  )
