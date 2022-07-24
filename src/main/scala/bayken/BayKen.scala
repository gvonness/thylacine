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
import bayken.config.visualisation.ProbabalisticVisualisationConfig
import thylacine.config._

import bayken.config.inference.MassInferenceConfig
import cats.effect.unsafe.implicits.global
import os.Path
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import pureconfig.module.enumeratum._

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

  lazy val visualisationConfigDefault: ProbabalisticVisualisationConfig =
    ConfigSource.default
      .at(baseConfigLabel + "visualisation-defaults")
      .loadOrThrow[ProbabalisticVisualisationConfig]

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
        "shinken001"
      )

    kenDataReferences.foreach { ref =>
      val kenConfig =
        ConfigSource
          .file((baseDataPath / (ref + ".conf")).toString)
          .at("shinken-data")
          .loadOrThrow[ShinkenConfig]

      if (kenConfig.recalculateOnNextRun) {
        val resultPath = baseResultPath / kenConfig.label
        os.makeDir.all(resultPath)

        NonAnalyticInference.runAndVisualize(kenConfig, resultPath)

      } else {
        println(s"Skipping recalculation of $ref data")
      }
    }
  }
}
