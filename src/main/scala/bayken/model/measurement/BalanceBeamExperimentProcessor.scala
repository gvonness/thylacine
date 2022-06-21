package ai.entrolution
package bayken.model.measurement

import bayken.config.MassInferenceConfig
import bayken.model.ken.BladeGeometry
import bayken.numerical._

case class BalanceBeamExperimentProcessor(
    bladeGeometry: BladeGeometry,
    inferenceParameters: MassInferenceConfig
) {

  private val bladePolesAndWeights: Seq[(Double, Double)] =
    bladeGeometry.tangArcLength match {
      case Some(tangArcLength) =>
        LegendreQuadrature(inferenceParameters.tangQuadratureSize)
          .getPolesAndWeights(0, tangArcLength) ++ LegendreQuadrature(
          inferenceParameters.bladeQuadratureSize
        ).getPolesAndWeights(tangArcLength, bladeGeometry.bladeArcLength)
      case None =>
        LegendreQuadrature(inferenceParameters.tangQuadratureSize)
          .getPolesAndWeights(0, bladeGeometry.tsubaBoundaryArcLengths.head) ++
          LegendreQuadrature(
            2
          ).getPolesAndWeights(bladeGeometry.tsubaBoundaryArcLengths.head,
                               bladeGeometry.tsubaBoundaryArcLengths.last
          ) ++ LegendreQuadrature(
            inferenceParameters.bladeQuadratureSize
          ).getPolesAndWeights(bladeGeometry.tsubaBoundaryArcLengths.last,
                               bladeGeometry.bladeArcLength
          )
    }

  private val rotationCalculator
      : AxesRotationAndOffset[PiecewisePolynomial1DSupport] =
    AxesRotationAndOffset(bladeGeometry.baseMuneModel,
                          inferenceParameters.rotationalFitTolerance
    )

  def totalMassMeasurement(
      kenMass: Double,
      massUncertainty: Double
  ): MeasurementRow =
    MeasurementRow(
      bladePolesAndWeights.map(_._2).toList,
      kenMass,
      massUncertainty
    )

  def processExperiment(
      measurement: BalanceExperimentMeasurement
  ): Seq[MeasurementRow] = {
    val fulcrumPoint =
      Point2D(measurement.fulcrumPosition, measurement.fulcrumHeight)
    val counterWeightPoint = Point2D(measurement.counterWeightPosition,
                                     measurement.counterWeightHeight
    )

    val rotationAngle: Double = rotationCalculator.getRotationCorrectionFor(
      fulcrumPoint,
      counterWeightPoint
    )

    val muneModel: PiecewisePolynomial1DSupport =
      bladeGeometry.rotateMuneModel(rotationAngle)

    def getCoefficients(fulcrumPos: Double): List[Double] = {
      val torqueIntegrand: RealValuedFunction =
        bladeGeometry.torqueIntegrand(fulcrumPosition = fulcrumPos, muneModel)

      bladePolesAndWeights
        .map(pw => torqueIntegrand.evalAt(pw._1) * pw._2)
        .toList
    }

    Seq(MeasurementRow(
          getCoefficients(fulcrumPoint.x),
          measurement.solveConstant,
          measurement.uncertainty
        ),
        MeasurementRow(
          getCoefficients(counterWeightPoint.x),
          measurement.dualSolveConstant,
          measurement.dualUncertainty
        )
    )
  }
}
