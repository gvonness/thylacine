ThisBuild / baseVersion := "0.4.1"

ThisBuild / organization := "ai.entrolution"
ThisBuild / organizationName := "Greg von Nessi"
ThisBuild / publishGithubUser := "gvonness"
ThisBuild / publishFullName := "Greg von Nessi"

ThisBuild / homepage := Some(url("https://github.com/gvonness/thylacine"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/gvonness/thylacine"), "git@github.com:gvonness/thylacine.git")
)

ThisBuild / startYear := Some(2020)
ThisBuild / endYear := Some(2023)

ThisBuild / spiewakCiReleaseSnapshots := false
ThisBuild / spiewakMainBranches := Seq("main")

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

scalaVersion := DependencyVersions.scala2p13Version
ThisBuild / crossScalaVersions := Seq(DependencyVersions.scala2p13Version)

Global / idePackagePrefix := Some("ai.entrolution")
Global / excludeLintKeys += idePackagePrefix

lazy val commonSettings = Seq(
  scalaVersion := DependencyVersions.scala2p13Version,
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-encoding",
    "UTF-8",
    "-Xlint:_",
    "-Ywarn-unused:-implicits",
    "-Ywarn-value-discard",
    "-Ywarn-dead-code"
  )
)

lazy val thylacine = (project in file("."))
  .enablePlugins(SonatypeCiReleasePlugin)
  .settings(
    commonSettings,
    name := "thylacine",
    libraryDependencies ++= Dependencies.thylacine,
    crossScalaVersions := Seq(
      DependencyVersions.scala2p13Version
    )
  )
