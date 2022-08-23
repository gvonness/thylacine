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

import thylacine.model.components.forwardmodel._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

import ai.entrolution.thylacine.model.distributions.CauchyDistribution

import java.util.UUID

case class CauchyLikelihood(
    private[thylacine] override val posteriorTermIdentifier: TermIdentifier,
    private[thylacine] val observations: RecordedData,
    private[thylacine] override val forwardModel: ForwardModel,
    private[thylacine] override val validated: Boolean = false
) extends Likelihood[ForwardModel, CauchyDistribution] {
  if (!validated) {
    assert(forwardModel.rangeDimension == observations.data.dimension)
  }

  private[thylacine] override lazy val getValidated: CauchyLikelihood =
    if (validated) {
      this
    } else {
      this.copy(observations = observations.getValidated, forwardModel = forwardModel.getValidated, validated = true)
    }

  private[thylacine] override lazy val observationModel: CauchyDistribution =
    CauchyDistribution(observations)

}

object CauchyLikelihood {

  def apply(
      forwardModel: ForwardModel,
      measurements: Vector[Double],
      uncertainties: Vector[Double]
  ): CauchyLikelihood =
    CauchyLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      observations = RecordedData(
        values = VectorContainer(measurements),
        symmetricConfidenceIntervals = VectorContainer(uncertainties)
      ),
      forwardModel = forwardModel
    )
}
