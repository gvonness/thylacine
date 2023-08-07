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
import org.apache.commons.math3.special.Gamma.gamma
import org.apache.commons.math3.util.FastMath

private[thylacine] case class CauchyDistribution(
  mean: VectorContainer,
  covariance: MatrixContainer,
  validated: Boolean = false
) extends Distribution
    with CanValidate[CauchyDistribution] {
  if (!validated) {
    assert(covariance.rowTotalNumber == covariance.columnTotalNumber)
    assert(covariance.rowTotalNumber == mean.dimension)
  }

  implicit private val randBasis: RandBasis = new RandBasis(
    new ThreadLocalRandomGenerator(new MersenneTwister(this.hashCode()))
  )

  override private[thylacine] lazy val getValidated: CauchyDistribution =
    if (validated) {
      this
    } else {
      CauchyDistribution(mean.getValidated, covariance.getValidated, validated = true)
    }

  private lazy val logMultiplier = Math.log(gamma((1 + domainDimension) / 2.0)) - Math.log(
    gamma(0.5)
  ) - domainDimension / 2.0 * Math.log(FastMath.PI) - Math.log(det(covariance.rawMatrix)) / 2.0

  override val domainDimension: Int = mean.dimension

  private lazy val rawInverseCovariance: DenseMatrix[Double] =
    inv(covariance.rawMatrix)

  override private[thylacine] def logPdfAt(
    input: VectorContainer
  ): Double = {
    val differentialFromMean = input.rawVector - mean.rawVector

    logMultiplier - (1.0 + domainDimension) / 2.0 * Math.log(
      1 + differentialFromMean.t * rawInverseCovariance * differentialFromMean
    )
  }

  override private[thylacine] def logPdfGradientAt(
    input: VectorContainer
  ): VectorContainer = {
    val differentialFromMean = input.rawVector - mean.rawVector
    val multiplierResult =
      (1 + domainDimension) / (1 + differentialFromMean.t * rawInverseCovariance * differentialFromMean)
    val vectorResult = rawInverseCovariance * differentialFromMean

    VectorContainer(multiplierResult * vectorResult)
  }

  private lazy val chiSquared = ChiSquared(1)

  // Leveraging connection to Gamma and Gaussian distributions
  private[thylacine] def getRawSample: VectorContainer =
    VectorContainer(
      MultivariateGaussian(mean.rawVector, covariance.rawMatrix / chiSquared.sample()).sample()
    )

}

private[thylacine] object CauchyDistribution {

  private[thylacine] def apply(input: RecordedData): CauchyDistribution = {
    val validatedData = input.getValidated

    CauchyDistribution(
      validatedData.data,
      validatedData.covariance,
      validated = true
    )
  }
}
