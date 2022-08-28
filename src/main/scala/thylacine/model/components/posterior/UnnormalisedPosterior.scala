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
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

import cats.effect.implicits._
import cats.syntax.all._

private[thylacine] trait UnnormalisedPosterior[F[_]] extends Posterior[F, Prior[F, _], Likelihood[F, _, _]] {
  this: AsyncImplicits[F] =>

  priors: Set[Prior[F, _]]
  likelihoods: Set[Likelihood[F, _, _]]

  private[thylacine] override final val isAnalytic: Boolean = false

  override private[thylacine] def logPdfAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, Double] =
    for {
      priorFib              <- priors.toList.parTraverse(_.logPdfAt(input)).start
      likelihoodFib         <- likelihoods.toList.parTraverse(_.logPdfAt(input)).start
      priorEvaluations      <- priorFib.joinWithNever
      likelihoodEvaluations <- likelihoodFib.joinWithNever
    } yield (priorEvaluations ++ likelihoodEvaluations).sum
}
