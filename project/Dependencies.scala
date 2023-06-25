import sbt._

object DependencyVersions {
  val scala2p13Version = "2.13.8"

  val bengalStmVersion           = "0.7.1-SNAPSHOT-28-aeec642"
  val bigMathVersion             = "2.3.0"
  val breezeVersion              = "2.0"
  val catsEffectVersion          = "3.3.14"
  val catsEffectTestingVersion   = "1.4.0"
  val commonMathVersion          = "3.6.1"
  val parallelCollectionsVersion = "1.0.4"
}

object Dependencies {
  import DependencyVersions._

  private val bengalStm: ModuleID =
    "ai.entrolution" %% "bengal-stm" % bengalStmVersion

  private val bigMath: ModuleID =
    "ch.obermuhlner" % "big-math" % bigMathVersion

  private val breeze: Seq[ModuleID] = Seq(
    "org.scalanlp" %% "breeze"         % breezeVersion,
    "org.scalanlp" %% "breeze-natives" % breezeVersion
  )

  private val commonMath: ModuleID =
    "org.apache.commons" % "commons-math3" % commonMathVersion

  private val catsEffect: ModuleID =
    "org.typelevel" %% "cats-effect" % catsEffectVersion

  private val catsEffectTesting: ModuleID =
    "org.typelevel" %% "cats-effect-testing-scalatest" % catsEffectTestingVersion % "test"

  private val parallelCollections: ModuleID =
    "org.scala-lang.modules" %% "scala-parallel-collections" % parallelCollectionsVersion

  val thylacine: Seq[ModuleID] =
    Seq(
      bengalStm,
      bigMath,
      catsEffect,
      catsEffectTesting,
      commonMath,
      parallelCollections
    ) ++ breeze
}
