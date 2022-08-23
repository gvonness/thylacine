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
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core._

import ai.entrolution.thylacine.model.core.values.modelparameters.{ModelParameterPdf, ModelParameterRawMappings}
import cats.effect.implicits._
import cats.implicits._

private[thylacine] trait Posterior[P <: Prior[_], L <: Likelihood[_, _]]
    extends ModelParameterPdf
    with ModelParameterRawMappings {
  private[thylacine] def priors: Set[P]
  private[thylacine] def likelihoods: Set[L]
  private[thylacine] def isAnalytic: Boolean

  override final val domainDimension =
    priors.toVector.map(_.domainDimension).sum

  protected override final lazy val orderedParameterIdentifiersWithDimension
      : ResultOrErrIo[Vector[(ModelParameterIdentifier, Int)]] =
    ResultOrErrIo.fromCalculation {
      priors.toVector
        .sortBy(_.posteriorTermIdentifier)
        .map(i =>
          ModelParameterIdentifier(
            i.posteriorTermIdentifier.value
          ) -> i.generatorDimension
        )
    }

  private[thylacine] override final def logPdfGradientAt(
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

  private[thylacine] def samplePriors: ResultOrErrIo[ModelParameterCollection] =
    for {
      sampleCollection <-
        priors.toVector.parTraverse(_.sampleModelParameters)
      result <-
        ResultOrErrIo.fromCalculation(sampleCollection.reduce(_ rawMergeWith _))
    } yield result
}
