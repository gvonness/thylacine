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
package thylacine.model.components.prior

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

private[thylacine] case class CauchyPrior(
    identifier: ModelParameterIdentifier,
    priorData: BelievedData,
    validated: Boolean = false
) extends Prior[CauchyBeliefModel] {

  protected override lazy val priorModel: CauchyBeliefModel =
    CauchyBeliefModel(priorData)

  private[thylacine] override lazy val getValidated: CauchyPrior =
    if (validated) this
    else this.copy(priorData = priorData.getValidated, validated = true)

  protected override def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation(priorModel.getRawSample)
}

object CauchyPrior {

  def apply(
      label: String,
      values: Vector[Double],
      confidenceIntervals: Vector[Double]
  ): CauchyPrior = {
    assert(values.size == confidenceIntervals.size)
    CauchyPrior(
      identifier = ModelParameterIdentifier(label),
      priorData = BelievedData(
        values = VectorContainer(values),
        symmetricConfidenceIntervals = VectorContainer(confidenceIntervals)
      )
    )
  }
}
