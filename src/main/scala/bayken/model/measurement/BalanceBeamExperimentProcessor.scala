package ai.entrolution
package bayken.model.measurement

import bayken.model.ken.ShinkenGeometry
import bayken.numerical._

// Should this just be merged into Blade Geometry?
case class BalanceBeamExperimentProcessor(
    bladeGeometry: ShinkenGeometry
) {

  private val rotationCalculator: AxesRotationAndOffset[PiecewisePolynomial1DSupport] =
    AxesRotationAndOffset(bladeGeometry.backEdgeModel,
                          bladeGeometry.kissakeSakiPoint.x,
                          bladeGeometry.inferenceConfig.rotationalFitTolerance
    )

  def totalMassMeasurement(
      kenMass: Double,
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      bladeGeometry.massInferencePolesAndWeights.map(_._2),
      kenMass,
      massUncertainty,
      cs => bladeGeometry.mapMassCoeffiencetsIntoQuadratureValues(cs)
    )

  // Tip is taken to have 0 mass per length
  def zeroMassAtTipMeasurement(
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      List.fill(bladeGeometry.massInferencePolesAndWeights.size - 1)(0d) :+ 1d,
      0d,
      massUncertainty / 100.0,
      cs => bladeGeometry.mapMassCoeffiencetsIntoQuadratureValues(cs)
    )

  // Mass is continuous at Kissake
  def massContinuousAroundKissake(
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      bladeGeometry.quadratureKissakeBoundaryIsolated,
      0d,
      massUncertainty / 100.0,
      cs => bladeGeometry.mapMassCoeffiencetsIntoQuadratureValues(cs)
    )

  def processExperiment(
      measurement: BalanceExperimentMeasurement
  ): Seq[MeasurementRow] = {
    val fulcrumPoint =
      Point2D(measurement.fulcrumPosition, measurement.fulcrumHeight)
    val counterWeightPoint = Point2D(measurement.counterWeightPosition, measurement.counterWeightHeight)

    val (rotationAngle: Double, fulcrumOrigin: Point2D, counterWeightOrigin: Point2D) = rotationCalculator
      .getRotationCorrectionFor(
        fulcrumPoint,
        counterWeightPoint,
        measurement.kissakeSakiPosition
      )
      .get

    def getCoefficients(fulcrumPos: Point2D): List[Double] = {
      val torqueIntegrand: RealValuedFunction =
        bladeGeometry.torqueIntegrand(fulcrumPosition = fulcrumPos, rotationAngle)

      bladeGeometry.massInferencePolesAndWeights
        .map(pw => torqueIntegrand.evalAt(pw._1) * pw._2)
    }

    Seq(MeasurementRow(
          getCoefficients(fulcrumOrigin),
          measurement.solveConstant,
          measurement.uncertainty,
          bladeGeometry.mapMassCoeffiencetsIntoQuadratureValues
        ),
        MeasurementRow(
          getCoefficients(counterWeightOrigin),
          measurement.dualSolveConstant,
          measurement.dualUncertainty,
          bladeGeometry.mapMassCoeffiencetsIntoQuadratureValues
        )
    )
  }
}
