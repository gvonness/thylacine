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

import thylacine.model.core.values.{ MatrixContainer, VectorContainer }
import thylacine.model.core.{ CanValidate, RecordedData }

import breeze.linalg._
import breeze.stats.distributions._
import org.apache.commons.math3.random.MersenneTwister

private[thylacine] case class GaussianDistribution(
  mean: VectorContainer,
  covariance: MatrixContainer,
  validated: Boolean = false
) extends Distribution
    with CanValidate[GaussianDistribution] {
  if (!validated) {
    assert(covariance.rowTotalNumber == covariance.columnTotalNumber)
    assert(covariance.rowTotalNumber == mean.dimension)
  }

  implicit private val randBasis: RandBasis = new RandBasis(
    new ThreadLocalRandomGenerator(new MersenneTwister(this.hashCode()))
  )

  private[thylacine] override lazy val getValidated: GaussianDistribution =
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
  ): Double =
    rawDistribution.logPdf(input.rawVector)

  private[thylacine] override def logPdfGradientAt(
    input: VectorContainer
  ): VectorContainer =
    VectorContainer(
      rawInverseCovariance * (mean.rawVector - input.rawVector)
    )

}

private[thylacine] object GaussianDistribution {

  private[thylacine] def apply(input: RecordedData): GaussianDistribution = {
    val validatedData = input.getValidated

    GaussianDistribution(
      validatedData.data,
      validatedData.covariance,
      validated = true
    )
  }
}
