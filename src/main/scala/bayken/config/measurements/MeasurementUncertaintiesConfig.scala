package ai.entrolution
package bayken.config.measurements

case class MeasurementUncertaintiesConfig(
    massUncertainty: Double,
    holderPositionUncertainty: Double,
    munePointXUncertainty: Option[Double],
    munePointYUncertainty: Option[Double],
    bladeWidthUncertainty: Option[Double]
)
