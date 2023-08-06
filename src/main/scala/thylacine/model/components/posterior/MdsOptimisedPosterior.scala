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

import thylacine.config.MdsConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.optimization.mds.MdsEngine

import cats.effect.kernel.Async

import scala.annotation.unused

case class MdsOptimisedPosterior[F[_]: Async](
  private[thylacine] val mdsConfig: MdsConfig,
  protected override val iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit],
  protected override val isConvergedCallback: Unit => F[Unit],
  private[thylacine] override val priors: Set[Prior[F, _]],
  private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]]
) extends AsyncImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with MdsEngine[F] {

  override protected val expansionMultiplier: Double =
    mdsConfig.expansionMultiplier

  override protected val contractionMultiplier: Double =
    mdsConfig.contractionMultiplier

  override protected val convergenceThreshold: Double =
    mdsConfig.convergenceThreshold

  override protected val numberOfPriorSamplesToSetStartingPoint: Int =
    mdsConfig.numberOfPriorSamplesToSetStartingPoint.getOrElse(100)
}

object MdsOptimisedPosterior {

  @unused
  def from[F[_]: Async](
    mdsConfig: MdsConfig,
    posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
    iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit],
    isConvergedCallback: Unit => F[Unit]
  ): MdsOptimisedPosterior[F] =
    MdsOptimisedPosterior(
      mdsConfig               = mdsConfig,
      iterationUpdateCallback = iterationUpdateCallback,
      isConvergedCallback     = isConvergedCallback,
      priors                  = posterior.priors,
      likelihoods             = posterior.likelihoods
    )
}
