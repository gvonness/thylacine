package ai.entrolution
package thylacine.model.distributions

import thylacine.model.core.CanValidate
import ai.entrolution.thylacine.model.core.values.VectorContainer

private[thylacine] case class UniformDistribution(
    upperBounds: VectorContainer,
    lowerBounds: VectorContainer,
    validated: Boolean = false
) extends Distribution
    with CanValidate[UniformDistribution] {

  private lazy val zippedBounds: ScalaVector[(Double, Double)] =
    lowerBounds.scalaVector.zip(upperBounds.scalaVector)

  if (!validated) {
    assert(upperBounds.dimension == lowerBounds.dimension)
    assert(!zippedBounds.exists(i => i._2 <= i._1))
  }

  private[thylacine] override lazy val getValidated: UniformDistribution =
    if (validated) {
      this
    } else {
      UniformDistribution(upperBounds.getValidated, lowerBounds.getValidated, validated = true)
    }

  override val domainDimension: Int = upperBounds.dimension

  private[thylacine] lazy val negLogVolume: ResultOrErrIo[Double] =
    ResultOrErrIo.fromValue {
      -zippedBounds.map { case (lowerBound, upperBound) =>
        Math.log(upperBound - lowerBound)
      }.sum
    }

  private lazy val negInfinity: ResultOrErrIo[Double] =
    ResultOrErrIo.fromValue(Double.NegativeInfinity)

  private[thylacine] lazy val zeroVector: ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromValue(VectorContainer.zeros(domainDimension))

  private[thylacine] def insideBounds(input: VectorContainer): Boolean =
    zippedBounds.zip(input.scalaVector).forall {
      case ((lowerBound, upperBound), value) if value >= lowerBound && value < upperBound =>
        true
      case _ =>
        false
    }

  private[thylacine] override def logPdfAt(
      input: VectorContainer
  ): ResultOrErrIo[Double] =
    if (insideBounds(input)) {
      negLogVolume
    } else {
      negInfinity
    }

  //We artificially set the gradient here to guide optimisers and
  //samplers using gradient information
  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation {
      VectorContainer {
        zippedBounds.zip(input.scalaVector).map {
          case ((lowerBound, _), value) if value < lowerBound =>
            lowerBound - value
          case ((_, upperBound), value) if value > upperBound =>
            upperBound - value
          case _ =>
            0d
        }
      }
    }

  private lazy val samplingScalingAndShift: ScalaVector[(Double, Double)] =
    zippedBounds.map { case (lowerBound, upperBound) =>
      (upperBound - lowerBound, lowerBound)
    }

  private[thylacine] def getRawSample: VectorContainer =
    VectorContainer(
      samplingScalingAndShift.par.map { case (scale, offset) =>
        Math.random() * scale + offset
      }.toVector
    )

}
