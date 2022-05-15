package ai.entrolution
package bayken.model.measurement

case class MeasurementRow(
    rowCoefficients: List[Double],
    solveConstant: Double,
    uncertainty: Double
)
