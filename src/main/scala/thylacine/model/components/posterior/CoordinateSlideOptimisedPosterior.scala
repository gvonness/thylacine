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

import bengal.stm.STM
import bengal.stm.model.TxnVar
import thylacine.config.CoordinateSlideConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.telemetry.HookeAndJeevesTelemetryUpdate
import thylacine.model.optimization.hookeandjeeves.CoordinateSlideEngine

import cats.effect.kernel.Async
import cats.syntax.all._

case class CoordinateSlideOptimisedPosterior[F[_]: STM: Async](
    private[thylacine] val coordinateSlideConfig: CoordinateSlideConfig,
    protected override val iterationUpdateCallback: HookeAndJeevesTelemetryUpdate => F[Unit],
    protected override val isConvergedCallback: Unit => F[Unit],
    private[thylacine] override val priors: Set[Prior[F, _]],
    private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]],
    protected override val currentBest: TxnVar[F, (Double, Vector[Double])],
    protected override val currentScale: TxnVar[F, Double],
    protected override val isConverged: TxnVar[F, Boolean]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
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

  def of[F[_]: STM: Async](
      coordinateSlideConfig: CoordinateSlideConfig,
      posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
      iterationUpdateCallback: HookeAndJeevesTelemetryUpdate => F[Unit],
      isConvergedCallback: Unit => F[Unit]
  ): F[CoordinateSlideOptimisedPosterior[F]] =
    for {
      currentBest  <- TxnVar.of((0d, Vector[Double]()))
      currentScale <- TxnVar.of(0d)
      isConverged  <- TxnVar.of(false)
    } yield CoordinateSlideOptimisedPosterior(
      coordinateSlideConfig = coordinateSlideConfig,
      iterationUpdateCallback = iterationUpdateCallback,
      isConvergedCallback = isConvergedCallback,
      priors = posterior.priors,
      likelihoods = posterior.likelihoods,
      currentBest = currentBest,
      currentScale = currentScale,
      isConverged = isConverged
    )
}
