package ai.entrolution
package bayken.config

case class MeasurementUncertaintiesConfig(
    massUncertaintyMultiplier: Double,
    massUncertaintyConstant: Double,
    positionUncertaintyConstant: Double
)
