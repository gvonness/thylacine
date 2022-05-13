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
import thylacine.model.components.prior.Prior
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

import java.util.UUID

case class GaussianLikelihood(
    posteriorTermIdentifier: TermIdentifier,
    observations: BelievedData,
    forwardModel: ForwardModel,
    modelParameterGenerators: Set[ModelParameterGenerator],
    validated: Boolean = false
) extends Likelihood[ForwardModel, GaussianBeliefModel] {
  if (!validated) {
    assert(
      modelParameterGenerators
        .map(_.generatorDimension)
        .sum == forwardModel.domainDimension
    )
    assert(forwardModel.rangeDimension == observations.data.dimension)
  }

  private[thylacine] override lazy val getValidated: GaussianLikelihood =
    if (validated) {
      this
    } else {
      this.copy(observations = observations.getValidated, validated = true)
    }

  private[thylacine] override lazy val observationModel: GaussianBeliefModel =
    GaussianBeliefModel(observations)

}

object GaussianLikelihood {

  def apply(
      forwardModel: ForwardModel,
      measurements: Vector[Double],
      uncertainties: Vector[Double],
      priors: Set[Prior[_]]
  ): GaussianLikelihood =
    GaussianLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      observations = BelievedData(
        values = VectorContainer(measurements),
        symmetricConfidenceIntervals = VectorContainer(uncertainties)
      ),
      forwardModel = forwardModel,
      modelParameterGenerators = priors.map(
        _.asInstanceOf[ModelParameterGenerator]
      ) // Can't avoid due to Sets not being covariant
    )
}
