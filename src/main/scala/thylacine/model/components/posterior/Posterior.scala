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
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.{ ModelParameterPdf, ModelParameterContext }

import cats.syntax.all._

private[thylacine] trait Posterior[F[_], P <: Prior[F, _], L <: Likelihood[F, _, _]]
    extends ModelParameterPdf[F]
    with ModelParameterContext {
  this: AsyncImplicits[F] =>

  private[thylacine] def priors: Set[P]
  private[thylacine] def likelihoods: Set[L]

  final override val domainDimension =
    priors.toVector.map(_.domainDimension).sum

  final override private[thylacine] lazy val orderedParameterIdentifiersWithDimension
    : Vector[(ModelParameterIdentifier, Int)] =
    priors.toVector
      .sortBy(_.posteriorTermIdentifier)
      .map(i =>
        ModelParameterIdentifier(
          i.posteriorTermIdentifier.value
        ) -> i.generatorDimension
      )

  final override private[thylacine] def logPdfGradientAt(
    input: ModelParameterCollection
  ): F[ModelParameterCollection] =
    for {
      priorSum <-
        priors.toList
          .traverse(_.logPdfGradientAt(input))
          .map(_.reduce(_ rawSumWith _))
      likelihoodSum <-
        likelihoods.toList
          .traverse(_.logPdfGradientAt(input))
          .map(_.reduce(_ rawSumWith _))
    } yield priorSum rawSumWith likelihoodSum

  private[thylacine] def samplePriors: F[ModelParameterCollection] =
    priors.toVector.traverse(_.sampleModelParameters(1).map(_.head)).map(_.reduce(_ rawMergeWith _))

  final override private[thylacine] def logPdfAt(
    input: ModelParameterCollection
  ): F[Double] =
    for {
      priorEvaluations      <- priors.toList.traverse(_.logPdfAt(input))
      likelihoodEvaluations <- likelihoods.toList.traverse(_.logPdfAt(input))
    } yield (priorEvaluations ++ likelihoodEvaluations).sum
}
