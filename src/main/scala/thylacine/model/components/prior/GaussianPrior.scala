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

import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.values.{MatrixContainer, VectorContainer}
import thylacine.model.distributions.GaussianDistribution

import breeze.stats.distributions.MultivariateGaussian
import cats.effect.kernel.Async

import scala.annotation.unused

case class GaussianPrior[F[_]: Async](
    private[thylacine] override val identifier: ModelParameterIdentifier,
    private[thylacine] val priorData: RecordedData,
    private[thylacine] override val validated: Boolean = false
) extends AsyncImplicits[F]
    with Prior[F, GaussianDistribution] {

  protected override lazy val priorDistribution: GaussianDistribution =
    GaussianDistribution(priorData)

  private lazy val rawDistribution: MultivariateGaussian =
    priorDistribution.rawDistribution

  private[thylacine] override lazy val getValidated: GaussianPrior[F] =
    if (validated) this
    else this.copy(priorData = priorData.getValidated, validated = true)

  protected override def rawSampleModelParameters: F[VectorContainer] =
    Async[F].delay(VectorContainer(rawDistribution.sample()))

  // Testing
  private[thylacine] lazy val mean: Vector[Double] =
    rawDistribution.mean.toScalaVector

  private[thylacine] lazy val covariance: Vector[Double] =
    rawDistribution.covariance.toArray.toVector

  lazy val entropy = rawDistribution.entropy
}

object GaussianPrior {

  def fromConfidenceIntervals[F[_]: Async](
      label: String,
      values: Vector[Double],
      confidenceIntervals: Vector[Double]
  ): GaussianPrior[F] = {
    assert(values.size == confidenceIntervals.size)
    GaussianPrior(
      identifier = ModelParameterIdentifier(label),
      priorData = RecordedData(
        values = VectorContainer(values),
        symmetricConfidenceIntervals = VectorContainer(confidenceIntervals)
      )
    )
  }

  @unused
  def fromCovarianceMatrix[F[_]: Async](
      label: String,
      values: Vector[Double],
      covarianceMatrix: Vector[Vector[Double]]
  ): GaussianPrior[F] = {
    val covarianceContainer = MatrixContainer(covarianceMatrix)
    val valueContainer      = VectorContainer(values)
    assert(covarianceContainer.isSquare && valueContainer.dimension == covarianceContainer.rowTotalNumber)
    GaussianPrior(
      identifier = ModelParameterIdentifier(label),
      priorData = RecordedData(
        data = valueContainer,
        covariance = covarianceContainer
      )
    )
  }
}
