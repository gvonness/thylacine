package ai.entrolution
package thylacine.model.distributions

import thylacine.model.core.CanValidate
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.VectorContainer

import cats.effect.kernel.Async

import scala.collection.immutable.{Vector => ScalaVector}
import scala.collection.parallel.CollectionConverters._

private[thylacine] case class UniformDistribution[F[_]: Async](
    upperBounds: VectorContainer,
    lowerBounds: VectorContainer,
    validated: Boolean = false
) extends Distribution[F]
    with CanValidate[UniformDistribution[F]] {

  private lazy val zippedBounds: ScalaVector[(Double, Double)] =
    lowerBounds.scalaVector.zip(upperBounds.scalaVector)

  if (!validated) {
    assert(upperBounds.dimension == lowerBounds.dimension)
    assert(!zippedBounds.exists(i => i._2 <= i._1))
  }

  private[thylacine] override lazy val getValidated: UniformDistribution[F] =
    if (validated) {
      this
    } else {
      UniformDistribution(upperBounds.getValidated, lowerBounds.getValidated, validated = true)
    }

  override val domainDimension: Int = upperBounds.dimension

  private[thylacine] lazy val negLogVolume: ResultOrErrF[F, Double] =
    (-zippedBounds.map { case (lowerBound, upperBound) =>
      Math.log(upperBound - lowerBound)
    }.sum).toResultM

  private lazy val negInfinity: ResultOrErrF[F, Double] =
    Double.NegativeInfinity.toResultM

  private[thylacine] lazy val zeroVector: ResultOrErrF[F, VectorContainer] =
    VectorContainer
      .zeros(domainDimension)
      .toResultM

  private[thylacine] def insideBounds(input: VectorContainer): Boolean =
    zippedBounds.zip(input.scalaVector).forall {
      case ((lowerBound, upperBound), value) if value >= lowerBound && value < upperBound =>
        true
      case _ =>
        false
    }

  private[thylacine] override def logPdfAt(
      input: VectorContainer
  ): ResultOrErrF[F, Double] =
    if (insideBounds(input)) {
      negLogVolume
    } else {
      negInfinity
    }

  //We artificially set the gradient here to guide optimisers and
  //samplers using gradient information
  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrF[F, VectorContainer] =
    VectorContainer {
      zippedBounds.zip(input.scalaVector).map {
        case ((lowerBound, _), value) if value < lowerBound =>
          lowerBound - value
        case ((_, upperBound), value) if value > upperBound =>
          upperBound - value
        case _ =>
          0d
      }
    }.toResultM

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
