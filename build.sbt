ThisBuild / baseVersion := "0.2.1"
ThisBuild / organization := "ai.entrolution"
ThisBuild / organizationName := "Greg von Nessi"
ThisBuild / publishGithubUser := "gvonness"
ThisBuild / publishFullName := "Greg von Nessi"
ThisBuild / startYear := Some(2020)
ThisBuild / endYear := Some(2022)

ThisBuild / homepage := Some(url("https://github.com/gvonness/thylacine"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/gvonness/thylacine"), "git@github.com:gvonness/thylacine.git")
)

ThisBuild / spiewakCiReleaseSnapshots := false
ThisBuild / spiewakMainBranches := Seq("main")

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

name := "thylacine"

scalaVersion := "2.13.8"
ThisBuild / crossScalaVersions := Seq("2.13.8")

Global / excludeLintKeys += idePackagePrefix

libraryDependencies += "org.scalanlp"           %% "breeze"                     % "2.0"
libraryDependencies += "org.scalanlp"           %% "breeze-natives"             % "2.0"
libraryDependencies += "ch.obermuhlner"          % "big-math"                   % "2.3.0"
libraryDependencies += "org.apache.commons"      % "commons-math3"              % "3.6.1"
libraryDependencies += "ai.entrolution"         %% "bengal-stm"                 % "0.3.9"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.typelevel"          %% "cats-effect"                % "3.3.12"
libraryDependencies += "org.scalatest"          %% "scalatest"                  % "3.2.12" % "test"

idePackagePrefix := Some("ai.entrolution")

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

enablePlugins(SonatypeCiReleasePlugin)
