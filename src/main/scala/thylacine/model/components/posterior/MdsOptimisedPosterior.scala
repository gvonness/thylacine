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
import bengal.stm.model.{TxnVar, TxnVarMap}
import thylacine.config.MdsConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.mds.{MdsEngine, MdsTelemetryUpdate, ModelParameterSimplex}

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.annotation.unused
import scala.collection.immutable.Queue

case class MdsOptimisedPosterior[F[_]: STM: Async](
    mdsConfig: MdsConfig,
    iterationUpdateCallback: MdsTelemetryUpdate => Unit = _ => (),
    isConvergedCallback: Unit => Unit = _ => (),
    override val priors: Set[Prior[F, _]],
    override val likelihoods: Set[Likelihood[F, _, _]],
    override protected val parallelismTokenPool: TxnVar[F, Int],
    override protected val queuedEvaluations: TxnVar[F, Queue[(Int, ModelParameterCollection)]],
    override protected val currentResults: TxnVarMap[F, Int, Double],
    override protected val unprocessedIndices: TxnVar[F, Set[Int]],
    override protected val currentBest: TxnVar[F, (Int, Double)],
    override protected val currentSimplex: TxnVar[F, ModelParameterSimplex],
    override protected val isConverged: TxnVar[F, Boolean]
) extends StmImplicits[F]
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
  def of[F[_]: STM: Async](
      mdsConfig: MdsConfig,
      posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
      iterationUpdateCallback: MdsTelemetryUpdate => Unit = _ => (),
      isConvergedCallback: Unit => Unit = _ => ()
  ): F[MdsOptimisedPosterior[F]] =
    for {
      parallelismTokenPool <- TxnVar.of(mdsConfig.evaluationParallelism)
      queuedEvaluations    <- TxnVar.of(Queue[(Int, ModelParameterCollection)]())
      currentResults       <- TxnVarMap.of(Map[Int, Double]())
      unprocessedIndices   <- TxnVar.of(Set[Int]())
      currentBest          <- TxnVar.of((0, Double.NegativeInfinity))
      currentSimplex <-
        TxnVar.of(
          ModelParameterSimplex.unitRegularCenteredOnZero(posterior)
        )
      isConverged <- TxnVar.of(false)
    } yield MdsOptimisedPosterior(
      mdsConfig = mdsConfig,
      iterationUpdateCallback = iterationUpdateCallback,
      isConvergedCallback = isConvergedCallback,
      priors = posterior.priors,
      likelihoods = posterior.likelihoods,
      parallelismTokenPool = parallelismTokenPool,
      queuedEvaluations = queuedEvaluations,
      currentResults = currentResults,
      unprocessedIndices = unprocessedIndices,
      currentBest = currentBest,
      currentSimplex = currentSimplex,
      isConverged = isConverged
    )
}
