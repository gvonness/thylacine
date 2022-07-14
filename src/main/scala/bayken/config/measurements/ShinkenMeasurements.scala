package ai.entrolution
package bayken.config.measurements

import bayken.model.measurement.BalanceExperimentMeasurement

case class ShinkenMeasurements(
    shinkenMass: Double,
    measurementUncertainties: MeasurementUncertaintiesConfig,
    balanceExperimentMeasurements: List[BalanceExperimentMeasurementConfig],
    pointMeasurements: List[BackEdgePointConfig]
) {

  lazy val measurementModels: Seq[BalanceExperimentMeasurement] =
    balanceExperimentMeasurements.map(bems => BalanceExperimentMeasurement(bems, this))
}
