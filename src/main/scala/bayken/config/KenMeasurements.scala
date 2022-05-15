package ai.entrolution
package bayken.config

case class KenMeasurements(
    kenLength: Double,
    kenMass: Double,
    measurementUncertainties: Option[MeasurementUncertaintiesConfig],
    balanceExperimentMeasurements: List[BalanceExperimentMeasurementConfig]
)
