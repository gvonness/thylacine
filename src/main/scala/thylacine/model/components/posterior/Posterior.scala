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
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.{ModelParameterPdf, ModelParameterRawMappings}

import cats.effect.implicits._
import cats.syntax.all._

private[thylacine] trait Posterior[F[_], P <: Prior[F, _], L <: Likelihood[F, _, _]]
    extends ModelParameterPdf[F]
    with ModelParameterRawMappings[F] {
  this: AsyncImplicits[F] =>

  private[thylacine] def priors: Set[P]
  private[thylacine] def likelihoods: Set[L]
  private[thylacine] def isAnalytic: Boolean

  override final val domainDimension =
    priors.toVector.map(_.domainDimension).sum

  protected override final lazy val orderedParameterIdentifiersWithDimension
      : ResultOrErrF[F, Vector[(ModelParameterIdentifier, Int)]] =
    priors.toVector
      .sortBy(_.posteriorTermIdentifier)
      .map(i =>
        ModelParameterIdentifier(
          i.posteriorTermIdentifier.value
        ) -> i.generatorDimension
      )
      .toResultM

  private[thylacine] override final def logPdfGradientAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, ModelParameterCollection[F]] =
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

  private[thylacine] def samplePriors: ResultOrErrF[F, ModelParameterCollection[F]] =
    for {
      sampleCollection <-
        priors.toVector.parTraverse(_.sampleModelParameters)
      result <-
        sampleCollection.reduce(_ rawMergeWith _).toResultM
    } yield result
}
