package ai.entrolution
package bayken.model.measurement

import bayken.model.ken.ShinkenGeometry
import bayken.numerical._

// Should this just be merged into Blade Geometry?
case class BalanceBeamExperimentProcessor(
    bladeGeometry: ShinkenGeometry
) {

  private val rotationCalculator: AxesRotationAndOffset[PiecewisePolynomial1DSupport] =
    AxesRotationAndOffset(bladeGeometry.backEdgeModel, bladeGeometry.inferenceConfig.rotationalFitTolerance)

  def totalMassMeasurement(
      kenMass: Double,
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      bladeGeometry.taggedMassInferencePolesAndWeights.map(_.weight).toVector,
      kenMass,
      massUncertainty
    )

  // Tip is taken to have 0 mass per length
  def zeroMassAtTipMeasurement(
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      Vector.fill(bladeGeometry.taggedMassInferencePolesAndWeights.size - 1)(0d) :+ 1d,
      0d,
      massUncertainty / 1000000.0
    )

  // Mass is continuous at Kissake
  def massContinuousAroundKissake(
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      bladeGeometry.quadratureKissakeBoundaryIsolated.toVector,
      0d,
      massUncertainty / 1000000.0
    )

  def processExperiment(
      measurement: BalanceExperimentMeasurement
  ): Seq[MeasurementRow] = {
    val fulcrumPoint =
      Point2D(measurement.fulcrumPosition, measurement.fulcrumHeight)
    val counterWeightPoint = Point2D(measurement.counterWeightPosition, measurement.counterWeightHeight)

    val (basePoint, measuredPoint) = (measurement.kissakeSakiPosition, measurement.nakagoJiriPosition) match {
      case (Some(pt), _) =>
        (bladeGeometry.kissakeSakiPoint.x, pt)
      case (_, Some(pt)) =>
        (bladeGeometry.nakogoJiriPoint.x, pt)
      case _ =>
        throw new RuntimeException("measurement missing geometry reference point")
    }

    val (rotationAngle: Double, fulcrumOrigin: Point2D, counterWeightOrigin: Point2D) = rotationCalculator
      .getRotationCorrectionFor(
        fulcrumPoint,
        counterWeightPoint,
        basePoint,
        measuredPoint
      )
      .get

    def getCoefficients(fulcrumPos: Point2D): Vector[Double] = {
      val torqueIntegrand: RealValuedFunction =
        bladeGeometry.torqueIntegrand(fulcrumPosition = fulcrumPos, rotationAngle)

      bladeGeometry.taggedMassInferencePolesAndWeights
        .map(pw => torqueIntegrand.evalAt(pw.pole) * pw.weight)
        .toVector
    }

    Seq(
      MeasurementRow(
        getCoefficients(fulcrumOrigin),
        measurement.solveConstant,
        measurement.uncertainty
      ),
      MeasurementRow(
        getCoefficients(counterWeightOrigin),
        measurement.dualSolveConstant,
        measurement.dualUncertainty
      )
    )
  }
}
