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
package thylacine.model.components.posterior

import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

import cats.effect.implicits._
import cats.implicits._

abstract class NonAnalyticPosterior(
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]]
) extends Posterior[Prior[_], Likelihood[_, _]] {
  override final val isAnalytic: Boolean = false

  override final def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      priorLogPdfSumFib <-
        priors.toList.parTraverse(_.logPdfAt(input)).map(_.sum).start
      likelihoodLogPdfSumFib <-
        likelihoods.toList.parTraverse(_.logPdfAt(input)).map(_.sum).start
      priorSum      <- priorLogPdfSumFib.joinWithNever
      likelihoodSum <- likelihoodLogPdfSumFib.joinWithNever
    } yield priorSum + likelihoodSum

  override final def logPdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    for {
      priorLogPdfGradSumFib <-
        priors.toList
          .parTraverse(_.logPdfGradientAt(input))
          .map(_.reduce(_ rawSumWith _))
          .start
      likelihoodLogPdfGradSumFib <-
        likelihoods.toList
          .parTraverse(_.logPdfGradientAt(input))
          .map(_.reduce(_ rawSumWith _))
          .start
      priorSum      <- priorLogPdfGradSumFib.joinWithNever
      likelihoodSum <- likelihoodLogPdfGradSumFib.joinWithNever
    } yield priorSum rawSumWith likelihoodSum
}
