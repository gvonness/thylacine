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

import bayken.config.measurements.{
  BalanceExperimentMeasurementConfig,
  MeasurementUncertaintiesConfig,
  ShinkenMeasurements
}

case class BalanceExperimentMeasurement(
    fulcrumPosition: Double,
    fulcrumHeight: Double,
    counterWeightPosition: Double,
    counterWeightHeight: Double,
    counterWeightMass: Double,
    kissakeSakiPosition: Option[Double],
    nakagoJiriPosition: Option[Double],
    shinkenMass: Double,
    uncertainties: MeasurementUncertaintiesConfig
) {
  assert(kissakeSakiPosition.isDefined ^ nakagoJiriPosition.isDefined)

  lazy val massUncertainty: Double =
    counterWeightMass + uncertainties.massUncertainty

  lazy val solveConstant: Double =
    (counterWeightPosition - fulcrumPosition) * counterWeightMass

  val uncertainty: Double =
    2 * uncertainties.holderPositionUncertainty * counterWeightMass + Math
      .abs(
        fulcrumPosition - counterWeightPosition
      ) * massUncertainty + 2 * massUncertainty * uncertainties.holderPositionUncertainty

  lazy val dualSolveConstant: Double =
    (fulcrumPosition - counterWeightPosition) * (shinkenMass - counterWeightMass)

  lazy val dualUncertainty: Double =
    2 * uncertainties.holderPositionUncertainty * (shinkenMass - counterWeightMass) + Math
      .abs(
        fulcrumPosition - counterWeightPosition
      ) * 2 * counterWeightMass + 4 * uncertainties.holderPositionUncertainty * massUncertainty
}

object BalanceExperimentMeasurement {

  def apply(
      rawMeasurement: BalanceExperimentMeasurementConfig,
      kenData: ShinkenMeasurements
  ): BalanceExperimentMeasurement =
    BalanceExperimentMeasurement(
      rawMeasurement.fulcrumPosition,
      rawMeasurement.fulcrumHeight.getOrElse(0d),
      rawMeasurement.counterWeightPosition,
      rawMeasurement.counterWeightHeight.getOrElse(0d),
      rawMeasurement.counterWeightMass,
      rawMeasurement.kissakeSakiPosition,
      rawMeasurement.nakagoJiriPosition,
      kenData.shinkenMass,
      kenData.measurementUncertainties
    )
}
