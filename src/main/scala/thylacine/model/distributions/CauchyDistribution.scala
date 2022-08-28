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
import cats.effect.syntax.all._
import cats.syntax.all._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.special.Gamma.gamma
import org.apache.commons.math3.util.FastMath

private[thylacine] case class CauchyDistribution[F[_]: Async](
    mean: VectorContainer,
    covariance: MatrixContainer,
    validated: Boolean = false
) extends Distribution[F]
    with CanValidate[CauchyDistribution[F]] {
  if (!validated) {
    assert(covariance.rowTotalNumber == covariance.columnTotalNumber)
    assert(covariance.rowTotalNumber == mean.dimension)
  }

  implicit private val randBasis: RandBasis = new RandBasis(
    new ThreadLocalRandomGenerator(new MersenneTwister(this.hashCode()))
  )

  private[thylacine] override lazy val getValidated: CauchyDistribution[F] =
    if (validated) {
      this
    } else {
      CauchyDistribution(mean.getValidated, covariance.getValidated, validated = true)
    }

  private lazy val multiplier = gamma((1 + domainDimension) / 2.0) / (gamma(
    0.5
  ) * math.pow(FastMath.PI, domainDimension / 2.0) * math.sqrt(
    det(rawInverseCovariance)
  ))

  override val domainDimension: Int = mean.dimension

  private lazy val rawInverseCovariance: DenseMatrix[Double] =
    inv(covariance.rawMatrix)

  private[thylacine] override def logPdfAt(
      input: VectorContainer
  ): ResultOrErrF[F, Double] =
    (for {
      diffVec <- Async[F].delay(input.rawVector - mean.rawVector)
      result <- Async[F].delay(
                  Math.pow(1 + diffVec.t * rawInverseCovariance * diffVec, (1.0 + domainDimension) / 2.0)
                )
    } yield multiplier * result).toResultM

  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrF[F, VectorContainer] =
    (for {
      diffVec <- Async[F].delay(input.rawVector - mean.rawVector)
      multiplierFib <- Async[F].delay {
                         -(1 + domainDimension) * (1 + diffVec.t * rawInverseCovariance * diffVec)
                       }.start
      vecFib           <- Async[F].delay(rawInverseCovariance * diffVec).start
      multiplierResult <- multiplierFib.joinWithNever
      vecResult        <- vecFib.joinWithNever
    } yield VectorContainer(
      multiplierResult * vecResult
    )).toResultM

  private lazy val chiSquared = ChiSquared(1)

  // Leveraging connection to Gamma and Gaussian distributions
  private[thylacine] def getRawSample: VectorContainer =
    VectorContainer(
      MultivariateGaussian(mean.rawVector, covariance.rawMatrix / chiSquared.sample()).sample()
    )

}

private[thylacine] object CauchyDistribution {

  private[thylacine] def apply[F[_]: Async](input: RecordedData): CauchyDistribution[F] = {
    val validatedData = input.getValidated

    CauchyDistribution(
      validatedData.data,
      validatedData.covariance,
      validated = true
    )
  }
}
