ThisBuild / organization := "online.aoxiang"
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / licenses += ("MIT", new java.net.URL("https://mit-license.org/"))
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tomls",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )
