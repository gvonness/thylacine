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
package thylacine.model.distributions

import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.{MatrixContainer, VectorContainer}
import thylacine.model.core.{CanValidate, RecordedData}

import breeze.linalg._
import breeze.stats.distributions._
import cats.effect.kernel.Async
import org.apache.commons.math3.random.MersenneTwister

private[thylacine] case class GaussianDistribution[F[_]: Async](
    mean: VectorContainer,
    covariance: MatrixContainer,
    validated: Boolean = false
) extends Distribution[F]
    with CanValidate[GaussianDistribution[F]] {
  if (!validated) {
    assert(covariance.rowTotalNumber == covariance.columnTotalNumber)
    assert(covariance.rowTotalNumber == mean.dimension)
  }

  implicit private val randBasis: RandBasis = new RandBasis(
    new ThreadLocalRandomGenerator(new MersenneTwister(this.hashCode()))
  )

  private[thylacine] override lazy val getValidated: GaussianDistribution[F] =
    if (validated) {
      this
    } else {
      GaussianDistribution(mean.getValidated, covariance.getValidated, validated = true)
    }

  override val domainDimension: Int = mean.dimension

  // Low-level API
  private[thylacine] lazy val rawDistribution: MultivariateGaussian =
    MultivariateGaussian(mean.rawVector, covariance.rawMatrix)

  private lazy val rawInverseCovariance: DenseMatrix[Double] =
    inv(covariance.rawMatrix)

  private[thylacine] override def logPdfAt(
      input: VectorContainer
  ): ResultOrErrF[F, Double] =
    rawDistribution.logPdf(input.rawVector).toResultM

  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrF[F, VectorContainer] =
    VectorContainer(
      rawInverseCovariance * (mean.rawVector - input.rawVector)
    ).toResultM

}

private[thylacine] object GaussianDistribution {

  private[thylacine] def apply[F[_]: Async](input: RecordedData): GaussianDistribution[F] = {
    val validatedData = input.getValidated

    GaussianDistribution(
      validatedData.data,
      validatedData.covariance,
      validated = true
    )
  }
}
