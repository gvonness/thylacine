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
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.sampling.hmcmc.HmcmcEngine

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.collection.immutable.Queue

case class HmcmcSampledPosterior[F[_]: STM: Async](
    private[thylacine] val hmcmcConfig: HmcmcConfig,
    protected override val epsilonUpdateCallback: Double => F[Unit],
    protected override val dhMonitorCallback: Double => F[Unit],
    protected override val processedSamplesCallback: Int => F[Unit],
    private[thylacine] val seed: Map[String, Vector[Double]],
    private[thylacine] override val priors: Set[Prior[F, _]],
    private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]],
    protected override val currentMcmcPosition: TxnVar[F, Option[ModelParameterCollection]],
    protected override val burnInComplete: TxnVar[F, Boolean],
    protected override val simulationEpsilon: TxnVar[F, Double],
    protected override val epsilonAdjustmentResults: TxnVar[F, Queue[Double]],
    protected override val currentlySampling: TxnVar[F, Boolean],
    protected override val numberOfSamplesProcessed: TxnVar[F, Int]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with HmcmcEngine[F] {

  override protected final val simulationsBetweenSamples: Int =
    hmcmcConfig.stepsBetweenSamples

  override protected final val stepsInSimulation: Int =
    hmcmcConfig.stepsInDynamicsSimulation

  override protected final val simulationInitialEpsilon: Double =
    hmcmcConfig.dynamicsSimulationStepSize

  override protected final val maxEpsilonHistory: Int =
    hmcmcConfig.maxStepSizeHistoryForAdjustment

  override protected final val targetAcceptance: Double =
    hmcmcConfig.targetAcceptanceRatio

  override protected final val warmUpSimulationCount: Int =
    hmcmcConfig.warmupStepCount

  override protected final val startingPoint: F[ModelParameterCollection] =
    Async[F].delay(IndexedVectorCollection(seed))
}

object HmcmcSampledPosterior {

  def of[F[_]: STM: Async](
      hmcmcConfig: HmcmcConfig,
      posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
      epsilonUpdateCallback: Double => F[Unit],
      dhMonitorCallback: Double => F[Unit],
      processedSamplesCallback: Int => F[Unit],
      seed: Map[String, Vector[Double]]
  ): F[HmcmcSampledPosterior[F]] =
    for {
      currentMcmcPosition      <- TxnVar.of[F, Option[ModelParameterCollection]](None)
      burnInComplete           <- TxnVar.of(false)
      simulationEpsilon        <- TxnVar.of(0.1)
      numberOfProcssedSamples  <- TxnVar.of(0)
      epsilonAdjustmentResults <- TxnVar.of(Queue[Double]())
      currentlySampling        <- TxnVar.of(false)
      posterior <- Async[F].delay {
                     HmcmcSampledPosterior(
                       hmcmcConfig = hmcmcConfig,
                       epsilonUpdateCallback = epsilonUpdateCallback,
                       dhMonitorCallback = dhMonitorCallback,
                       processedSamplesCallback = processedSamplesCallback,
                       seed = seed,
                       priors = posterior.priors,
                       likelihoods = posterior.likelihoods,
                       currentMcmcPosition = currentMcmcPosition,
                       burnInComplete = burnInComplete,
                       simulationEpsilon = simulationEpsilon,
                       epsilonAdjustmentResults = epsilonAdjustmentResults,
                       currentlySampling = currentlySampling,
                       numberOfSamplesProcessed = numberOfProcssedSamples
                     )
                   }
      _ <- posterior.launchInitialisation
      _ <- posterior.waitForInitialisationCompletion
    } yield posterior

}
