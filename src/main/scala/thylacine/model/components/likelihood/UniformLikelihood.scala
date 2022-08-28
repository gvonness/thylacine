package ai.entrolution
package thylacine.model.components.likelihood

import thylacine.model.components.forwardmodel.ForwardModel
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.values.VectorContainer
import thylacine.model.distributions
import thylacine.model.distributions.UniformDistribution

import cats.effect.kernel.Async

import java.util.UUID

case class UniformLikelihood[F[_]: Async, T <: ForwardModel[F]](
    private[thylacine] override val posteriorTermIdentifier: TermIdentifier,
    private[thylacine] val upperBounds: VectorContainer,
    private[thylacine] val lowerBounds: VectorContainer,
    private[thylacine] override val forwardModel: T,
    private[thylacine] override val validated: Boolean = false
) extends AsyncImplicits[F]
    with Likelihood[F, T, UniformDistribution[F]] {
  if (!validated) {
    assert(lowerBounds.dimension == upperBounds.dimension)
  }

  private[thylacine] override lazy val getValidated: UniformLikelihood[F, T] =
    if (validated) {
      this
    } else {
      this.copy(
        lowerBounds = lowerBounds.getValidated,
        upperBounds = upperBounds.getValidated,
        validated = true
      )
    }

  private[thylacine] override lazy val observationModel: UniformDistribution[F] =
    distributions.UniformDistribution(upperBounds = upperBounds, lowerBounds = lowerBounds)

}

object UniformLikelihood {

  def apply[F[_]: Async, T <: ForwardModel[F]](
      forwardModel: T,
      upperBounds: Vector[Double],
      lowerBounds: Vector[Double]
  ): UniformLikelihood[F, T] =
    UniformLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      upperBounds = VectorContainer(upperBounds),
      lowerBounds = VectorContainer(lowerBounds),
      forwardModel = forwardModel
    )
}
