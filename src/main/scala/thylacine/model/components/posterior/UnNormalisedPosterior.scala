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

import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.Erratum.{ResultOrErrF, ResultOrErrIo}
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection

import cats.effect.implicits._
import cats.implicits._

case class UnNormalisedPosterior(
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]]
) extends NonAnalyticPosterior(priors, likelihoods) {

  override private[thylacine] def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      priorFib              <- priors.toList.traverse(_.logPdfAt(input)).start
      likelihoodFib         <- likelihoods.toList.traverse(_.logPdfAt(input)).start
      priorEvaluations      <- priorFib.joinWithNever
      likelihoodEvaluations <- likelihoodFib.joinWithNever
    } yield (priorEvaluations ++ likelihoodEvaluations).sum
}
