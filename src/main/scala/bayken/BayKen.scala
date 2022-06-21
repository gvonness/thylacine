/*
 * Copyright 2020-2022 Greg von Nessi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.entrolution
package bayken

import bayken.config._
import bayken.config.measurements.MeasurementUncertaintiesConfig
import bayken.config.visualisation.MassInferenceVisualisationConfig
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

  lazy val visualisationConfigDefault: MassInferenceVisualisationConfig =
    ConfigSource.default
      .at(baseConfigLabel + "visualisation-defaults")
      .loadOrThrow[MassInferenceVisualisationConfig]

  lazy val inferenceConfigDefault: MassInferenceConfig =
    ConfigSource.default
      .at(baseConfigLabel + "inference-defaults")
      .loadOrThrow[MassInferenceConfig]

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
