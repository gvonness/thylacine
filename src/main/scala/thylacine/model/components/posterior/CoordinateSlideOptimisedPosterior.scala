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

import thylacine.config.CoordinateSlideConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.optimization.hookeandjeeves.CoordinateSlideEngine

import cats.effect.kernel.Async

case class CoordinateSlideOptimisedPosterior[F[_]: Async](
  private[thylacine] val coordinateSlideConfig: CoordinateSlideConfig,
  override protected val iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit],
  override protected val isConvergedCallback: Unit => F[Unit],
  override private[thylacine] val priors: Set[Prior[F, ?]],
  override private[thylacine] val likelihoods: Set[Likelihood[F, ?, ?]]
) extends AsyncImplicits[F]
    with Posterior[F, Prior[F, ?], Likelihood[F, ?, ?]]
    with CoordinateSlideEngine[F] {

  override protected val convergenceThreshold: Double =
    coordinateSlideConfig.convergenceThreshold

  override protected val goldenSectionTolerance: Double =
    coordinateSlideConfig.goldenSectionTolerance

  override protected val lineProbeExpansionFactor: Double =
    coordinateSlideConfig.lineProbeExpansionFactor

  override protected val numberOfSamplesToSetScale: Int =
    coordinateSlideConfig.numberOfPriorSamplesToSetScale.getOrElse(100)
}

object CoordinateSlideOptimisedPosterior {

  def apply[F[_]: Async](
    coordinateSlideConfig: CoordinateSlideConfig,
    posterior: Posterior[F, Prior[F, ?], Likelihood[F, ?, ?]],
    iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit],
    isConvergedCallback: Unit => F[Unit]
  ): CoordinateSlideOptimisedPosterior[F] =
    CoordinateSlideOptimisedPosterior(
      coordinateSlideConfig   = coordinateSlideConfig,
      iterationUpdateCallback = iterationUpdateCallback,
      isConvergedCallback     = isConvergedCallback,
      priors                  = posterior.priors,
      likelihoods             = posterior.likelihoods
    )
}
