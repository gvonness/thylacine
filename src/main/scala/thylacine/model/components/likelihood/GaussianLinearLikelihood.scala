/*
 * Copyright 2020-2023 Greg von Nessi
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

import bengal.stm.STM
import thylacine.model.components.forwardmodel._
import thylacine.model.components.prior._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.values.VectorContainer
import thylacine.model.distributions.GaussianDistribution

import cats.effect.kernel.Async
import cats.syntax.all._

import java.util.UUID
import scala.annotation.unused

// Gaussian linear likelihoods are one of the few likelihoods that can lead to an analytic posterior.
// Thus, it gets a dedicated case class that is leveraged to do an analytic check in the posterior
// construction.
case class GaussianLinearLikelihood[F[_]: Async](
    private[thylacine] override val posteriorTermIdentifier: TermIdentifier,
    private[thylacine] val observations: RecordedData,
    private[thylacine] override val forwardModel: LinearForwardModel[F],
    private[thylacine] override val validated: Boolean = false
) extends AsyncImplicits[F]
    with Likelihood[F, LinearForwardModel[F], GaussianDistribution] {
  if (!validated) {
    assert(forwardModel.rangeDimension == observations.data.dimension)
  }

  private[thylacine] override lazy val getValidated: GaussianLinearLikelihood[F] =
    if (validated) {
      this
    } else {
      this.copy(observations = observations.getValidated, validated = true)
    }

  private[thylacine] override lazy val observationDistribution: GaussianDistribution =
    GaussianDistribution(observations)

}

object GaussianLinearLikelihood {

  @unused
  def of[F[_]: STM: Async](
      coefficients: Vector[Vector[Double]],
      measurements: Vector[Double],
      uncertainties: Vector[Double],
      prior: Prior[F, _],
      evalCacheDepth: Option[Int]
  ): F[GaussianLinearLikelihood[F]] =
    for {
      linearForwardModel <- LinearForwardModel
                              .of[F](
                                identifier = prior.identifier,
                                values = coefficients,
                                evalCacheDepth = evalCacheDepth
                              )
    } yield GaussianLinearLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      observations = RecordedData(
        values = VectorContainer(measurements),
        symmetricConfidenceIntervals = VectorContainer(uncertainties)
      ),
      forwardModel = linearForwardModel
    )
}
