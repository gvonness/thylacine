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
import thylacine.config.HmcmcConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.telemetry.HmcmcTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.sampling.hmcmc.HmcmcEngine

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.annotation.unused

case class HmcmcSampledPosterior[F[_]: STM: Async](
  private[thylacine] val hmcmcConfig: HmcmcConfig,
  protected override val telemetryUpdateCallback: HmcmcTelemetryUpdate => F[Unit],
  private[thylacine] val seed: Map[String, Vector[Double]],
  private[thylacine] override val priors: Set[Prior[F, _]],
  private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]],
  protected override val currentResult: TxnVar[F, Option[ModelParameterCollection]],
  protected override val burnInComplete: TxnVar[F, Boolean],
  protected override val numberOfSamplesRemaining: TxnVar[F, Int],
  protected override val jumpAcceptances: TxnVar[F, Int],
  protected override val jumpAttempts: TxnVar[F, Int]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with HmcmcEngine[F] {

  override protected final val simulationsBetweenSamples: Int =
    hmcmcConfig.stepsBetweenSamples

  override protected final val stepsInSimulation: Int =
    hmcmcConfig.stepsInDynamicsSimulation

  override protected final val simulationEpsilon: Double =
    hmcmcConfig.dynamicsSimulationStepSize

  override protected final val warmUpSimulationCount: Int =
    hmcmcConfig.warmupStepCount

  override protected final val startingPoint: F[ModelParameterCollection] =
    Async[F].delay(IndexedVectorCollection(seed))
}

@unused
object HmcmcSampledPosterior {

  @unused
  def of[F[_]: STM: Async](
    hmcmcConfig: HmcmcConfig,
    posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
    telemetryUpdateCallback: HmcmcTelemetryUpdate => F[Unit],
    seed: Map[String, Vector[Double]]
  ): F[HmcmcSampledPosterior[F]] =
    for {
      currentResult            <- TxnVar.of(None.asInstanceOf[Option[ModelParameterCollection]])
      burnInComplete           <- TxnVar.of(false)
      numberOfSamplesRemaining <- TxnVar.of(0)
      jumpAcceptances          <- TxnVar.of(0)
      jumpAttempts             <- TxnVar.of(0)
      posterior <- Async[F].delay {
                     HmcmcSampledPosterior(
                       hmcmcConfig              = hmcmcConfig,
                       telemetryUpdateCallback  = telemetryUpdateCallback,
                       seed                     = seed,
                       priors                   = posterior.priors,
                       likelihoods              = posterior.likelihoods,
                       currentResult            = currentResult,
                       burnInComplete           = burnInComplete,
                       jumpAcceptances          = jumpAcceptances,
                       jumpAttempts             = jumpAttempts,
                       numberOfSamplesRemaining = numberOfSamplesRemaining
                     )
                   }
      _ <- posterior.launchInitialisation
      _ <- posterior.waitForInitialisationCompletion
    } yield posterior

}
