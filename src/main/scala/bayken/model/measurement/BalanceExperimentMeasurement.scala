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
package bayken.model.measurement

import bayken.config._

import ai.entrolution.bayken.config.measurements.{
  BalanceExperimentMeasurementConfig,
  KenMeasurements,
  MeasurementUncertaintiesConfig
}

case class BalanceExperimentMeasurement(
    fulcrumPosition: Double,
    fulcrumHeight: Double,
    counterWeightPosition: Double,
    counterWeightHeight: Double,
    counterWeightMass: Double,
    kenMass: Double,
    uncertainties: MeasurementUncertaintiesConfig
) {

  lazy val massUncertainty: Double =
    counterWeightMass + uncertainties.massUncertainty

  lazy val solveConstant: Double =
    (counterWeightPosition - fulcrumPosition) * counterWeightMass

  val uncertainty: Double =
    2 * uncertainties.positionUncertainty * counterWeightMass + Math
      .abs(
        fulcrumPosition - counterWeightPosition
      ) * massUncertainty + 2 * massUncertainty * uncertainties.positionUncertainty

  lazy val dualSolveConstant: Double =
    (fulcrumPosition - counterWeightPosition) * (kenMass - counterWeightMass)

  lazy val dualUncertainty: Double =
    2 * uncertainties.positionUncertainty * (kenMass - counterWeightMass) + Math
      .abs(
        fulcrumPosition - counterWeightPosition
      ) * 2 * counterWeightMass + 4 * uncertainties.positionUncertainty * massUncertainty
}

object BalanceExperimentMeasurement {

  def apply(
      rawMeasurement: BalanceExperimentMeasurementConfig,
      kenData: KenMeasurements
  ): BalanceExperimentMeasurement =
    BalanceExperimentMeasurement(
      rawMeasurement.fulcrumPosition,
      rawMeasurement.fulcrumHeight,
      rawMeasurement.counterWeightPosition,
      rawMeasurement.counterWeightHeight,
      rawMeasurement.counterWeightMass,
      kenData.kenMass,
      kenData.measurementUncertainties
    )
}
