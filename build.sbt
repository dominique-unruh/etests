import sbt.librarymanagement
import sbt.librarymanagement.CrossVersion.{for2_13Use3, for3Use2_13}

import java.nio.file.{Path, Paths}

name := """etests"""

version := "1.0-SNAPSHOT"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.6.4"

//lazy val root = project in file(".")

lazy val webapp = (project in file("webapp"))
  .settings(
    libraryDependencies += guice,
    //    libraryDependencies += "io.github.classgraph" % "classgraph" % "4.8.181",
    libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test,
  )
  .enablePlugins(PlayScala)
  .dependsOn(problems)
  .dependsOn(exams)

lazy val problems = (project in file("problems"))
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    libraryDependencies += "org.commonmark" % "commonmark" % "0.25.1",
    libraryDependencies += "org.commonmark" % "commonmark-ext-gfm-tables" % "0.25.1",
    //    libraryDependencies += "com.github.benoitlouy" %% "indent" % "0.8.0",
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.5",
    libraryDependencies += "dev.scalapy" %% "scalapy-core" % "0.5.3",
    libraryDependencies += "com.eed3si9n.eval" % "eval" % "0.3.1" cross CrossVersion.full,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.2.1",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.14.0",
    libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "2.0.0",
    libraryDependencies += "com.github.cb372" %% "scalacache-core" % "0.28.0" cross for3Use2_13,
    libraryDependencies += "com.github.cb372" %% "scalacache-caffeine" % "0.28.0" cross for3Use2_13,
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.5",
    //      libraryDependencies += "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
    //      libraryDependencies += "com.github.cb372" %% "scalacache-ehcache" % "0.28.0" cross for3Use2_13,
    //      libraryDependencies += "org.ehcache" % "ehcache" % "3.10.8"
    libraryDependencies += "org.rocksdb" % "rocksdbjni" % "10.2.1",
    libraryDependencies += "org.jsoup" % "jsoup" % "1.17.2",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.14",
      "io.circe" %% "circe-generic" % "0.14.14",
      "io.circe" %% "circe-parser" % "0.14.14"
    ),
    libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.19",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-codec" % "1.19",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-dom" % "1.19",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-svg-dom" % "1.19",
    libraryDependencies += "io.github.classgraph" % "classgraph" % "4.8.181",
    libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.0", // Used to make Play happy with the nextcloud-api jackson dependency
    libraryDependencies += "org.aarboard.nextcloud" % "nextcloud-api" % "13.1.0" exclude("org.slf4j", "slf4j-simple")
  )

lazy val exams = (project in file("exams"))
  .settings (
      Compile / scalaSource := baseDirectory.value,
      Compile / excludeFilter := {
        val broken = (baseDirectory.value / "broken").toPath
        val test = (baseDirectory.value / "test").toPath
        new SimpleFileFilter(p => p.toPath.startsWith(broken) || p.toPath.startsWith(test)) || HiddenFileFilter
      },
      Test / scalaSource := baseDirectory.value / "test",
//      Test / excludeFilter := new SimpleFileFilter(_.toPath.startsWith((baseDirectory.value / "broken").toPath)) || HiddenFileFilter,
  )
  .dependsOn(problems)

