/*
 * Copyright 2020-2023 Greg von Nessi
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

import thylacine.config.ConjugateGradientConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.optimization.gradientdescent.ConjugateGradientEngine

import cats.effect.kernel.Async

case class ConjugateGradientOptimisedPosterior[F[_]: Async](
    private[thylacine] val gradientDescentConfig: ConjugateGradientConfig,
    protected override val newMaximumCallback: Double => F[Unit],
    protected override val isConvergedCallback: Unit => F[Unit],
    private[thylacine] override val priors: Set[Prior[F, _]],
    private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]]
) extends AsyncImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with ConjugateGradientEngine[F] {

  override protected val convergenceThreshold: Double =
    gradientDescentConfig.convergenceThreshold

  override protected val numberOfResultsToRetain: Int =
    gradientDescentConfig.numberOfResultsToRetain

  override protected val goldenSectionTolerance: Double =
    gradientDescentConfig.goldenSectionTolerance

  override protected val lineProbeExpansionFactor: Double =
    gradientDescentConfig.lineProbeExpansionFactor
}

object ConjugateGradientOptimisedPosterior {

  def from[F[_]: Async](
                         conjugateGradientConfig: ConjugateGradientConfig,
                         posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
                         newMaximumCallback: Double => F[Unit],
                         isConvergedCallback: Unit => F[Unit]
  ): ConjugateGradientOptimisedPosterior[F] =
    ConjugateGradientOptimisedPosterior(
      gradientDescentConfig = conjugateGradientConfig,
      newMaximumCallback = newMaximumCallback,
      isConvergedCallback = isConvergedCallback,
      priors = posterior.priors,
      likelihoods = posterior.likelihoods
    )
}
