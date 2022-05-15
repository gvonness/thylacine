package ai.entrolution
package bayken

import bayken.config._
import thylacine.config._

import cats.effect.unsafe.implicits.global
import os.Path
import pureconfig.ConfigSource
import pureconfig.generic.auto._

object BayKen {
  val baseConfigLabel      = "bayken."
  val baseConfigPath: Path = os.pwd / "src" / "main" / "resources"

  val baseDataPath: Path =
    os.pwd / "src" / "main" / "resources" / "dataDecrypted"
  val baseResultPath: Path = os.pwd / "src" / "main" / "results"

  lazy val uncertaintiesConfigDefault: MeasurementUncertaintiesConfig =
    ConfigSource.default
      .at(baseConfigLabel + "uncertainties-defaults")
      .loadOrThrow[MeasurementUncertaintiesConfig]

  lazy val visualisationConfigDefault: VisualisationConfig =
    ConfigSource.default
      .at(baseConfigLabel + "visualisation-defaults")
      .loadOrThrow[VisualisationConfig]

  lazy val inferenceConfigDefault: InferenceConfig =
    ConfigSource.default
      .at(baseConfigLabel + "inference-defaults")
      .loadOrThrow[InferenceConfig]

  lazy val slqSamplingConfig: SlqConfig =
    ConfigSource.default
      .at(baseConfigLabel + "slq-sampling")
      .loadOrThrow[SlqConfig]

  def main(args: Array[String]): Unit = {
    val kenDataReferences: List[String] =
      List(
        "shinken001",
        "shinken002",
        "shinken003",
        "shinken004",
        "mogito001"
      )

    kenDataReferences.foreach { ref =>
      val kenConfig =
        ConfigSource
          .file((baseDataPath / (ref + ".conf")).toString)
          .at("kendata")
          .loadOrThrow[KenConfig]

      if (kenConfig.recalculateAnalyticOnNextRun) {
        val resultPath = baseResultPath / kenConfig.label
        os.makeDir.all(resultPath)

        AnalyticInference.runAndVisualize(kenConfig,
                                          uncertaintiesConfigDefault,
                                          inferenceConfigDefault,
                                          visualisationConfigDefault,
                                          resultPath
        )

      } else {
        println(s"Skipping recalculation of $ref data")
      }
    }
  }
}
