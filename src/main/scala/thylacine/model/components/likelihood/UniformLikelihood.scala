/*
 * Copyright 2023 Greg von Nessi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.entrolution
package thylacine.model.components.likelihood

import thylacine.model.components.forwardmodel.ForwardModel
import thylacine.model.core.GenericIdentifier.*
import thylacine.model.core.*
import thylacine.model.core.values.VectorContainer
import thylacine.model.distributions
import thylacine.model.distributions.UniformDistribution

import cats.effect.kernel.Async

import java.util.UUID
import scala.annotation.unused

case class UniformLikelihood[F[_]: Async, T <: ForwardModel[F]](
  override private[thylacine] val posteriorTermIdentifier: TermIdentifier,
  private[thylacine] val upperBounds: VectorContainer,
  private[thylacine] val lowerBounds: VectorContainer,
  override private[thylacine] val forwardModel: T,
  override private[thylacine] val validated: Boolean = false
) extends AsyncImplicits[F]
    with Likelihood[F, T, UniformDistribution] {
  if (!validated) {
    assert(lowerBounds.dimension == upperBounds.dimension)
  }

  override private[thylacine] lazy val getValidated: UniformLikelihood[F, T] =
    if (validated) {
      this
    } else {
      this.copy(
        lowerBounds = lowerBounds.getValidated,
        upperBounds = upperBounds.getValidated,
        validated   = true
      )
    }

  override private[thylacine] lazy val observationDistribution: UniformDistribution =
    distributions.UniformDistribution(upperBounds = upperBounds, lowerBounds = lowerBounds)

}

@unused
object UniformLikelihood {

  def apply[F[_]: Async, T <: ForwardModel[F]](
    forwardModel: T,
    upperBounds: Vector[Double],
    lowerBounds: Vector[Double]
  ): UniformLikelihood[F, T] =
    UniformLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      upperBounds             = VectorContainer(upperBounds),
      lowerBounds             = VectorContainer(lowerBounds),
      forwardModel            = forwardModel
    )
}
