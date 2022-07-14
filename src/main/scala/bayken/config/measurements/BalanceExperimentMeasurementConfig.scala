package ai.entrolution
package bayken.config.measurements

case class BalanceExperimentMeasurementConfig(
    fulcrumPosition: Double,
    fulcrumHeight: Option[Double],
    counterWeightPosition: Double,
    counterWeightHeight: Option[Double],
    counterWeightMass: Double,
    kissakeSakiPosition: Option[Double],
    kashiraPosition: Option[Double]
)
