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
import thylacine.config.LeapfrogMcmcConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.sampling.SampleRequest
import thylacine.model.sampling.leapfrog.LeapfrogMcmcEngine

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.annotation.unused
import scala.collection.immutable.Queue

case class LeapfrogMcmcSampledPosterior[F[_]: STM: Async](
  private[thylacine] val leapfrogMcmcConfig: LeapfrogMcmcConfig,
  protected val distanceCalculation: (Vector[Double], Vector[Double]) => Double,
  protected val sampleRequestSetCallback: Int => F[Unit],
  protected val sampleRequestUpdateCallback: Int => F[Unit],
  private[thylacine] val seed: Map[String, Vector[Double]],
  private[thylacine] val priors: Set[Prior[F, _]],
  private[thylacine] val likelihoods: Set[Likelihood[F, _, _]],
  protected val sampleRequests: TxnVar[F, Queue[SampleRequest[F]]],
  protected val burnInComplete: TxnVar[F, Boolean],
  protected val interSampleDistanceCalculationResults: TxnVar[F, Vector[Vector[Double]]],
  protected val sampleLogPdfs: TxnVar[F, Vector[Double]],
  protected val samplePool: TxnVar[F, Vector[Vector[Double]]],
  protected val currentChainTallies: TxnVar[F, Vector[Int]]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with LeapfrogMcmcEngine[F] {

  final override protected val stepsBetweenSamples: Int =
    leapfrogMcmcConfig.stepsBetweenSamples

  final override protected val warmUpSimulationCount: Int =
    leapfrogMcmcConfig.warmupStepCount

  final override protected val samplePoolSize: Int =
    leapfrogMcmcConfig.samplePoolSize

  final override protected val startingPoint: F[ModelParameterCollection] =
    Async[F].delay(IndexedVectorCollection(seed))
}

@unused
object LeapfrogMcmcSampledPosterior {

  @unused
  def of[F[_]: STM: Async](
    leapfrogMcmcConfig: LeapfrogMcmcConfig,
    distanceCalculation: (Vector[Double], Vector[Double]) => Double,
    posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
    sampleRequestSetCallback: Int => F[Unit],
    sampleRequestUpdateCallback: Int => F[Unit],
    seed: Map[String, Vector[Double]]
  ): F[LeapfrogMcmcSampledPosterior[F]] =
    for {
      sampleRequests                        <- TxnVar.of(Queue[SampleRequest[F]]())
      burnInComplete                        <- TxnVar.of(false)
      interSampleDistanceCalculationResults <- TxnVar.of(Vector(Vector[Double]()))
      sampleLogPdfs                         <- TxnVar.of(Vector[Double]())
      samplePool                            <- TxnVar.of(Vector(Vector[Double]()))
      currentChainTallies                   <- TxnVar.of(Vector[Int]())
      posterior <- Async[F].delay {
                     LeapfrogMcmcSampledPosterior(
                       leapfrogMcmcConfig                    = leapfrogMcmcConfig,
                       distanceCalculation                   = distanceCalculation,
                       sampleRequestSetCallback              = sampleRequestSetCallback,
                       sampleRequestUpdateCallback           = sampleRequestUpdateCallback,
                       seed                                  = seed,
                       priors                                = posterior.priors,
                       likelihoods                           = posterior.likelihoods,
                       sampleRequests                        = sampleRequests,
                       burnInComplete                        = burnInComplete,
                       interSampleDistanceCalculationResults = interSampleDistanceCalculationResults,
                       sampleLogPdfs                         = sampleLogPdfs,
                       samplePool                            = samplePool,
                       currentChainTallies                   = currentChainTallies
                     )
                   }
      _ <- posterior.launchInitialisation
      _ <- posterior.waitForInitialisationCompletion
    } yield posterior

}
