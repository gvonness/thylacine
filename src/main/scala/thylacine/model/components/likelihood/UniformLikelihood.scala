package ai.entrolution
package thylacine.model.components.likelihood

import thylacine.model.components.forwardmodel.ForwardModel
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

import java.util.UUID

case class UniformLikelihood[T <: ForwardModel](
    private[thylacine] override val posteriorTermIdentifier: TermIdentifier,
    private[thylacine] val upperBounds: VectorContainer,
    private[thylacine] val lowerBounds: VectorContainer,
    private[thylacine] override val forwardModel: T,
    private[thylacine] override val validated: Boolean = false
) extends Likelihood[T, UniformBeliefModel] {
  if (!validated) {
    assert(lowerBounds.dimension == upperBounds.dimension)
  }

  private[thylacine] override lazy val getValidated: UniformLikelihood[T] =
    if (validated) {
      this
    } else {
      this.copy(
        lowerBounds = lowerBounds.getValidated,
        upperBounds = upperBounds.getValidated,
        validated = true
      )
    }

  private[thylacine] override lazy val observationModel: UniformBeliefModel =
    UniformBeliefModel(upperBounds = upperBounds, lowerBounds = lowerBounds)

}

object UniformLikelihood {

  def apply[T <: ForwardModel](
      forwardModel: T,
      upperBounds: Vector[Double],
      lowerBounds: Vector[Double]
  ): UniformLikelihood[T] =
    UniformLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      upperBounds = VectorContainer(upperBounds),
      lowerBounds = VectorContainer(lowerBounds),
      forwardModel = forwardModel
    )
}
