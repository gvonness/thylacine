package ai.entrolution
package bayken.model.measurement

import bayken.config._

case class BalanceExperimentMeasurement(
    fulcrumPosition: Double,
    counterWeightPosition: Double,
    counterWeightMass: Double,
    kenMass: Double,
    uncertainties: MeasurementUncertaintiesConfig
) {

  lazy val massUncertainty: Double =
    uncertainties.massUncertaintyMultiplier * counterWeightMass + uncertainties.massUncertaintyConstant

  lazy val solveConstant: Double =
    (counterWeightPosition - fulcrumPosition) * counterWeightMass

  val uncertainty: Double =
    2 * uncertainties.positionUncertaintyConstant * counterWeightMass + Math
      .abs(
        fulcrumPosition - counterWeightPosition
      ) * massUncertainty + 2 * massUncertainty * uncertainties.positionUncertaintyConstant

  lazy val dualSolveConstant: Double =
    (fulcrumPosition - counterWeightPosition) * (kenMass - counterWeightMass)

  lazy val dualUncertainty: Double =
    2 * uncertainties.positionUncertaintyConstant * (kenMass - counterWeightMass) + Math
      .abs(
        fulcrumPosition - counterWeightPosition
      ) * 2 * counterWeightMass + 4 * uncertainties.positionUncertaintyConstant * massUncertainty
}

object BalanceExperimentMeasurement {

  def apply(
      rawMeasurement: BalanceExperimentMeasurementConfig,
      uncertainties: MeasurementUncertaintiesConfig,
      kenData: KenMeasurements
  ): BalanceExperimentMeasurement =
    BalanceExperimentMeasurement(
      rawMeasurement.fulcrumPosition,
      rawMeasurement.counterWeightPosition,
      rawMeasurement.counterWeightMass,
      kenData.kenMass,
      uncertainties
    )
}
