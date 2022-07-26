/*
 * Copyright 2020-2022 Greg von Nessi
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

import cats.effect.IO

import java.util.UUID

// Gaussian linear likelihoods are one of the few likelihoods that can lead to an analytic posterior.
// Thus, it gets a dedicated case class that is leveraged to do an analytic check in the posterior
// construction.
case class GaussianLinearLikelihood(
    private[thylacine] override val posteriorTermIdentifier: TermIdentifier,
    private[thylacine] override val observations: BelievedData,
    private[thylacine] override val forwardModel: LinearForwardModel,
    private[thylacine] override val validated: Boolean = false
) extends Likelihood[LinearForwardModel, GaussianBeliefModel] {
  if (!validated) {
    assert(forwardModel.rangeDimension == observations.data.dimension)
  }

  private[thylacine] override lazy val getValidated: GaussianLinearLikelihood =
    if (validated) {
      this
    } else {
      this.copy(observations = observations.getValidated, validated = true)
    }

  private[thylacine] override lazy val observationModel: GaussianBeliefModel =
    GaussianBeliefModel(observations)

}

object GaussianLinearLikelihood {

  def apply(
      coefficients: Vector[Vector[Double]],
      measurements: Vector[Double],
      uncertainties: Vector[Double],
      prior: Prior[_],
      maxResultsToCache: Int
  )(implicit stm: STM[IO]): GaussianLinearLikelihood =
    GaussianLinearLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      observations = BelievedData(
        values = VectorContainer(measurements),
        symmetricConfidenceIntervals = VectorContainer(uncertainties)
      ),
      forwardModel = LinearForwardModel(
        identifier = prior.identifier,
        values = coefficients,
        maxResultsToCache = maxResultsToCache
      )
    )
}
