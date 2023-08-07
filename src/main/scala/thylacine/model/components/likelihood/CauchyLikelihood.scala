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
import thylacine.model.core.values.VectorContainer
import thylacine.model.distributions.CauchyDistribution

import cats.effect.kernel.Async

import java.util.UUID
import scala.annotation.unused

case class CauchyLikelihood[F[_]: Async](
  override private[thylacine] val posteriorTermIdentifier: TermIdentifier,
  private[thylacine] val observations: RecordedData,
  override private[thylacine] val forwardModel: ForwardModel[F],
  override private[thylacine] val validated: Boolean = false
) extends AsyncImplicits[F]
    with Likelihood[F, ForwardModel[F], CauchyDistribution] {
  if (!validated) {
    assert(forwardModel.rangeDimension == observations.data.dimension)
  }

  override private[thylacine] lazy val getValidated: CauchyLikelihood[F] =
    if (validated) {
      this
    } else {
      this.copy(observations = observations.getValidated, forwardModel = forwardModel.getValidated, validated = true)
    }

  override private[thylacine] lazy val observationDistribution: CauchyDistribution =
    CauchyDistribution(observations)

}

@unused
object CauchyLikelihood {

  def apply[F[_]: Async](
    forwardModel: ForwardModel[F],
    measurements: Vector[Double],
    uncertainties: Vector[Double]
  ): CauchyLikelihood[F] =
    CauchyLikelihood(
      posteriorTermIdentifier = TermIdentifier(UUID.randomUUID().toString),
      observations = RecordedData(
        values                       = VectorContainer(measurements),
        symmetricConfidenceIntervals = VectorContainer(uncertainties)
      ),
      forwardModel = forwardModel
    )
}
