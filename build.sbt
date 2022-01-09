ThisBuild / organization := "online.aoxiang"
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / licenses += ("MIT", new java.net.URL("https://mit-license.org/"))
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / publishTo := Some(("aoxiang-repository-snapshots" at "https://repo.aoxiang.online/snapshots"))
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / ".credentials")


lazy val root = project
  .in(file("."))
  .settings(
    name := "tomls",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-parse" % "0.3.6",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test",
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test"
    )
  )
