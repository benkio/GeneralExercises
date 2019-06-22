import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organizationName := "setgame"

lazy val root = (project in file("."))
  .settings(
    name := "SetGame",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "2.0.0-M4",
      "org.typelevel" %% "cats-core" % "2.0.0-M4",
      "com.github.julien-truffaut" %%  "monocle-core"  % "1.6.0-RC1",
      "com.github.julien-truffaut" %%  "monocle-macro"  % "1.6.0-RC1",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    )
  )
