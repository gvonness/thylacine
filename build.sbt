ThisBuild / baseVersion := "0.0.1"
ThisBuild / organization := "ai.entrolution"
ThisBuild / organizationName := "Greg von Nessi"
ThisBuild / publishGithubUser := "gvonness"
ThisBuild / publishFullName := "Greg von Nessi"
ThisBuild / startYear := Some(2020)
ThisBuild / endYear := Some(2022)

ThisBuild / homepage := Some(url("https://github.com/gvonness/thylacine"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/gvonness/thylacine"),
          "git@github.com:gvonness/thylacine.git"
  )
)

ThisBuild / spiewakCiReleaseSnapshots := false
ThisBuild / spiewakMainBranches := Seq("main")

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

name := "thylacine"

scalaVersion := "2.13.8"
ThisBuild / crossScalaVersions := Seq("2.13.8")

Global / excludeLintKeys += idePackagePrefix

libraryDependencies += "org.scalanlp"           %% "breeze"                     % "1.2"
libraryDependencies += "org.scalanlp"           %% "breeze-natives"             % "1.2"
libraryDependencies += "ch.obermuhlner"          % "big-math"                   % "2.3.0"
libraryDependencies += "ai.entrolution"         %% "bengal-stm"                 % "0.3.7"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.typelevel"          %% "cats-free"                  % "2.7.0"
libraryDependencies += "org.typelevel"          %% "cats-effect"                % "3.3.4"
libraryDependencies += "org.scalatest"          %% "scalatest"                  % "3.2.11" % "test"

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
