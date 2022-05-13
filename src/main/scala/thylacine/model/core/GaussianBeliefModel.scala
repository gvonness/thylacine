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
package thylacine.model.core

import thylacine.model.core.Erratum._

import breeze.linalg._
import breeze.stats.distributions._

private[thylacine] case class GaussianBeliefModel(
    mean: VectorContainer,
    covariance: MatrixContainer,
    validated: Boolean = false
) extends BeliefModel
    with CanValidate[GaussianBeliefModel] {
  if (!validated) {
    assert(covariance.rowTotalNumber == covariance.columnTotalNumber)
    assert(covariance.rowTotalNumber == mean.dimension)
  }

  private[thylacine] override lazy val getValidated: GaussianBeliefModel =
    if (validated) {
      this
    } else {
      GaussianBeliefModel(mean.getValidated,
                          covariance.getValidated,
                          validated = true
      )
    }

  private[thylacine] override val domainDimension: Int = mean.dimension

  // Low-level API
  private[thylacine] lazy val rawDistribution: MultivariateGaussian =
    MultivariateGaussian(mean.rawVector, covariance.rawMatrix)

  private lazy val rawInverseCovariance: DenseMatrix[Double] =
    inv(covariance.rawMatrix)

  private[thylacine] override def logPdfAt(input: VectorContainer): ResultOrErrIo[Double] =
    ResultOrErrIo.fromCalculation(rawDistribution.logPdf(input.rawVector))

  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation(
      VectorContainer(
        rawInverseCovariance * (mean.rawVector - input.rawVector)
      )
    )

}

private[thylacine] object GaussianBeliefModel {

  private[thylacine] def apply(input: BelievedData): GaussianBeliefModel = {
    val validatedData = input.getValidated

    GaussianBeliefModel(
      validatedData.data,
      validatedData.covariance,
      validated = true
    )
  }
}
