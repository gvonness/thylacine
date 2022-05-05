/*
 * Copyright 2020-2021 Entrolution
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
package thylacine.model.components.prior

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

import breeze.stats.distributions.MultivariateGaussian

case class GaussianPrior(
    identifier: ModelParameterIdentifier,
    priorData: BelievedData,
    validated: Boolean = false
) extends Prior[GaussianBeliefModel] {

  override lazy val priorModel: GaussianBeliefModel =
    GaussianBeliefModel(priorData)

  private lazy val rawDistribution: MultivariateGaussian =
    priorModel.rawDistribution

  override lazy val getValidated: GaussianPrior =
    if (validated) this
    else this.copy(priorData = priorData.getValidated, validated = true)

  override protected def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation(VectorContainer(rawDistribution.sample()))
}

object GaussianPrior {

  def apply(
      label: String,
      values: List[Double],
      confidenceIntervals: List[Double]
  ): GaussianPrior = {
    assert(values.size == confidenceIntervals.size)
    GaussianPrior(
      identifier = ModelParameterIdentifier(label),
      priorData = BelievedData(
        values = VectorContainer(values),
        symmetricConfidenceIntervals = VectorContainer(confidenceIntervals)
      )
    )
  }
}
