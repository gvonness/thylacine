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
package thylacine.model.components.prior

import thylacine.model.core.GenericIdentifier.*
import thylacine.model.core.*
import thylacine.model.core.values.VectorContainer
import thylacine.model.distributions.CauchyDistribution

import cats.effect.kernel.Async

import scala.annotation.unused

case class CauchyPrior[F[_]: Async](
  override private[thylacine] val identifier: ModelParameterIdentifier,
  private[thylacine] val priorData: RecordedData,
  override private[thylacine] val validated: Boolean = false
) extends AsyncImplicits[F]
    with Prior[F, CauchyDistribution] {

  override protected lazy val priorDistribution: CauchyDistribution =
    CauchyDistribution(priorData)

  override private[thylacine] lazy val getValidated: CauchyPrior[F] =
    if (validated) this
    else this.copy(priorData = priorData.getValidated, validated = true)

  override protected def rawSampleModelParameters: F[VectorContainer] =
    Async[F].delay(priorDistribution.getRawSample)
}

@unused
object CauchyPrior {

  def apply[F[_]: Async](
    label: String,
    values: Vector[Double],
    confidenceIntervals: Vector[Double]
  ): CauchyPrior[F] = {
    assert(values.size == confidenceIntervals.size)
    CauchyPrior(
      identifier = ModelParameterIdentifier(label),
      priorData = RecordedData(
        values                       = VectorContainer(values),
        symmetricConfidenceIntervals = VectorContainer(confidenceIntervals)
      )
    )
  }
}
