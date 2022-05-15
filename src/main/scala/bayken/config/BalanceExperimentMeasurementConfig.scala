package ai.entrolution
package bayken.config

case class BalanceExperimentMeasurementConfig(
    fulcrumPosition: Double,
    counterWeightPosition: Double,
    counterWeightMass: Double,
    massUncertaintyMultiplier: Option[Double] = None,
    massUncertaintyConstant: Option[Double] = None,
    positionUncertainty: Option[Double] = None
)
