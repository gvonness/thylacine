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
import thylacine.model.sampling.hmcmc.HmcmcEngine.SampleRequest

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.collection.immutable.Queue

case class HmcmcSampledPosterior[F[_]: STM: Async](
    hmcmcConfig: HmcmcConfig,
    sampleRequestSetCallback: Int => Unit = _ => (),
    sampleRequestUpdateCallback: Int => Unit = _ => (),
    epsilonUpdateCallback: Double => Unit = _ => (),
    dhMonitorCallback: Double => Unit = _ => (),
    seed: Map[String, Vector[Double]],
    override val priors: Set[Prior[F, _]],
    override val likelihoods: Set[Likelihood[F, _, _]],
    override protected val sampleRequests: TxnVar[F, Queue[SampleRequest[F]]],
    override protected val currentMcmcPositions: TxnVar[F, Queue[ModelParameterCollection]],
    override protected val burnInComplete: TxnVar[F, Boolean],
    override protected val simulationEpsilon: TxnVar[F, Double],
    override protected val epsilonAdjustmentResults: TxnVar[F, Queue[Double]],
    override protected val parallelismTokenPool: TxnVar[F, Int]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with HmcmcEngine[F] {

  override protected final val sampleParallelism: Int =
    hmcmcConfig.sampleParallelism.getOrElse(
      Math
        .max(Math.ceil(Runtime.getRuntime.availableProcessors() / 2.0), 1)
        .toInt
    )

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
      inputPosterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
      sampleRequestSetCallback: Int => Unit = _ => (),
      sampleRequestUpdateCallback: Int => Unit = _ => (),
      epsilonUpdateCallback: Double => Unit = _ => (),
      dhMonitorCallback: Double => Unit = _ => (),
      seedSpec: F[Map[String, Vector[Double]]]
  ): F[HmcmcSampledPosterior[F]] =
    for {
      sampleRequests           <- TxnVar.of(Queue[SampleRequest[F]]())
      currentMcmcPositions     <- TxnVar.of(Queue[ModelParameterCollection]())
      burnInComplete           <- TxnVar.of(false)
      simulationEpsilon        <- TxnVar.of(0.1)
      epsilonAdjustmentResults <- TxnVar.of(Queue[Double]())
      parallelismTokenPool     <- TxnVar.of(Runtime.getRuntime.availableProcessors())
      seed                     <- seedSpec
      posterior <- Async[F].delay {
                     HmcmcSampledPosterior(
                       hmcmcConfig = hmcmcConfig,
                       sampleRequestSetCallback = sampleRequestSetCallback,
                       sampleRequestUpdateCallback = sampleRequestUpdateCallback,
                       epsilonUpdateCallback = epsilonUpdateCallback,
                       dhMonitorCallback = dhMonitorCallback,
                       seed = seed,
                       priors = inputPosterior.priors,
                       likelihoods = inputPosterior.likelihoods,
                       sampleRequests = sampleRequests,
                       currentMcmcPositions = currentMcmcPositions,
                       burnInComplete = burnInComplete,
                       simulationEpsilon = simulationEpsilon,
                       epsilonAdjustmentResults = epsilonAdjustmentResults,
                       parallelismTokenPool = parallelismTokenPool
                     )
                   }
      _ <- posterior.initialise
      _ <- posterior.waitForInitialisation
    } yield posterior

}
