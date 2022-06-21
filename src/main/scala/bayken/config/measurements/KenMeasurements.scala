package ai.entrolution
package bayken.config.measurements

import bayken.model.measurement.BalanceExperimentMeasurement

case class KenMeasurements(
    kenLength: Double,
    kenMass: Double,
    components: KenComponentConfig,
    measurementUncertainties: MeasurementUncertaintiesConfig,
    balanceExperimentMeasurements: List[BalanceExperimentMeasurementConfig]
) {

  lazy val measurementModels: Seq[BalanceExperimentMeasurement] =
    balanceExperimentMeasurements.map(bems =>
      BalanceExperimentMeasurement(bems, this)
    )
}
