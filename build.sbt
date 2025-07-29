import sbt.librarymanagement
import sbt.librarymanagement.CrossVersion.{for2_13Use3, for3Use2_13}

name := """assessments"""

version := "1.0-SNAPSHOT"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.6.4"

//lazy val root = project in file(".")

lazy val webapp = (project in file("webapp"))
  .settings(
      libraryDependencies += guice,
      libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
  )
  .enablePlugins(PlayScala)
  .dependsOn(problems)

lazy val problems = (project in file("problems"))
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    libraryDependencies += "org.commonmark" % "commonmark" % "0.25.0",
    //    libraryDependencies += "com.github.benoitlouy" %% "indent" % "0.8.0",
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.5",
    libraryDependencies += "dev.scalapy" %% "scalapy-core" % "0.5.3",
    libraryDependencies += "com.eed3si9n.eval" % "eval" % "0.3.1" cross CrossVersion.full,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.2.1",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.14.0",
    libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2",
    libraryDependencies += "org.log4s" %% "log4s" % "1.10.0",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "2.0.0",
    libraryDependencies += "com.github.cb372" %% "scalacache-core" % "0.28.0" cross for3Use2_13,
    libraryDependencies += "com.github.cb372" %% "scalacache-caffeine" % "0.28.0" cross for3Use2_13,
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.4",
//      libraryDependencies += "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
//      libraryDependencies += "com.github.cb372" %% "scalacache-ehcache" % "0.28.0" cross for3Use2_13,
//      libraryDependencies += "org.ehcache" % "ehcache" % "3.10.8"
      libraryDependencies += "org.rocksdb" % "rocksdbjni" % "10.2.1",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.14",
      "io.circe" %% "circe-generic" % "0.14.14",
      "io.circe" %% "circe-parser" % "0.14.14"
    )
  )
