/*
 * Copyright 2023 Greg von Nessi
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

import thylacine.config.HookeAndJeevesConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.optimization.hookeandjeeves.HookeAndJeevesEngine

import cats.effect.kernel.Async

case class HookeAndJeevesOptimisedPosterior[F[_]: Async](
  private[thylacine] val hookeAndJeevesConfig: HookeAndJeevesConfig,
  override protected val iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit],
  override protected val isConvergedCallback: Unit => F[Unit],
  override private[thylacine] val priors: Set[Prior[F, ?]],
  override private[thylacine] val likelihoods: Set[Likelihood[F, ?, ?]]
) extends AsyncImplicits[F]
    with Posterior[F, Prior[F, ?], Likelihood[F, ?, ?]]
    with HookeAndJeevesEngine[F] {

  override protected val convergenceThreshold: Double =
    hookeAndJeevesConfig.convergenceThreshold

  override protected val numberOfSamplesToSetScale: Int =
    hookeAndJeevesConfig.numberOfPriorSamplesToSetScale.getOrElse(100)

}

object HookeAndJeevesOptimisedPosterior {

  def apply[F[_]: Async](
    hookeAndJeevesConfig: HookeAndJeevesConfig,
    posterior: Posterior[F, Prior[F, ?], Likelihood[F, ?, ?]],
    iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit],
    isConvergedCallback: Unit => F[Unit]
  ): HookeAndJeevesOptimisedPosterior[F] =
    HookeAndJeevesOptimisedPosterior(
      hookeAndJeevesConfig    = hookeAndJeevesConfig,
      iterationUpdateCallback = iterationUpdateCallback,
      isConvergedCallback     = isConvergedCallback,
      priors                  = posterior.priors,
      likelihoods             = posterior.likelihoods
    )
}
