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
import thylacine.config.HookeAndJeevesConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.optimization.hookeandjeeves.HookeAndJeevesEngine

import cats.effect.kernel.Async
import cats.syntax.all._

case class HookeAndJeevesOptimisedPosterior[F[_]: STM: Async](
    private[thylacine] val hookeAndJeevesConfig: HookeAndJeevesConfig,
    protected override val newMaximumCallback: Double => F[Unit],
    protected override val newScaleCallback: Double => F[Unit],
    protected override val isConvergedCallback: Unit => F[Unit],
    private[thylacine] override val priors: Set[Prior[F, _]],
    private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]],
    protected override val currentBest: TxnVar[F, (Double, Vector[Double])],
    protected override val currentScale: TxnVar[F, Double],
    protected override val isConverged: TxnVar[F, Boolean]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with HookeAndJeevesEngine[F] {

  override protected val convergenceThreshold: Double =
    hookeAndJeevesConfig.convergenceThreshold

  override protected val numberOfSamplesToSetScale: Int =
    hookeAndJeevesConfig.numberOfPriorSamplesToSetScale.getOrElse(100)

}

object HookeAndJeevesOptimisedPosterior {

  def of[F[_]: STM: Async](
      hookeAndJeevesConfig: HookeAndJeevesConfig,
      posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
      newMaximumCallback: Double => F[Unit],
      newScaleCallback: Double => F[Unit],
      isConvergedCallback: Unit => F[Unit]
  ): F[HookeAndJeevesOptimisedPosterior[F]] =
    for {
      currentBest  <- TxnVar.of((0d, Vector[Double]()))
      currentScale <- TxnVar.of(0d)
      isConverged  <- TxnVar.of(false)
    } yield HookeAndJeevesOptimisedPosterior(
      hookeAndJeevesConfig = hookeAndJeevesConfig,
      newMaximumCallback = newMaximumCallback,
      newScaleCallback = newScaleCallback,
      isConvergedCallback = isConvergedCallback,
      priors = posterior.priors,
      likelihoods = posterior.likelihoods,
      currentBest = currentBest,
      currentScale = currentScale,
      isConverged = isConverged
    )
}
