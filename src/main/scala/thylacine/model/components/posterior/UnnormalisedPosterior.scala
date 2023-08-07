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

import thylacine.model.components.forwardmodel.ForwardModel
import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.{ AsyncImplicits, CanValidate }
import thylacine.model.distributions.Distribution

import cats.effect.kernel.Async

case class UnnormalisedPosterior[F[_]: Async](
  override private[thylacine] val priors: Set[Prior[F, _]],
  override private[thylacine] val likelihoods: Set[Likelihood[F, _, _]],
  override private[thylacine] val validated: Boolean = false
) extends AsyncImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with CanValidate[UnnormalisedPosterior[F]] {
  if (!validated) {
    // Ensure there are no conflicting identifiers.
    assert(priors.size == priors.map(_.identifier).size)
    assert(
      likelihoods.size == likelihoods.map(_.posteriorTermIdentifier).size
    )
    assert(
      priors
        .map(_.posteriorTermIdentifier)
        .intersect(likelihoods.map(_.posteriorTermIdentifier))
        .isEmpty
    )
  }

  override private[thylacine] lazy val getValidated: UnnormalisedPosterior[F] =
    if (validated) {
      this
    } else {
      UnnormalisedPosterior(
        priors      = priors.map(_.getValidated.asInstanceOf[Prior[F, Distribution]]),
        likelihoods = likelihoods.map(_.getValidated.asInstanceOf[Likelihood[F, ForwardModel[F], Distribution]]),
        validated   = true
      )
    }
}
