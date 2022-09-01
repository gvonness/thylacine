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
import thylacine.config.MdsConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.telemetry.MdsTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.mds.{MdsEngine, ModelParameterSimplex}

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.annotation.unused
import scala.collection.immutable.Queue

case class MdsOptimisedPosterior[F[_]: STM: Async](
    private[thylacine] val mdsConfig: MdsConfig,
    protected override val iterationUpdateCallback: MdsTelemetryUpdate => F[Unit],
    protected override val isConvergedCallback: Unit => F[Unit],
    private[thylacine] override val priors: Set[Prior[F, _]],
    private[thylacine] override val likelihoods: Set[Likelihood[F, _, _]],
    protected override val parallelismTokenPool: TxnVar[F, Int],
    protected override val queuedEvaluations: TxnVar[F, Queue[(Int, ModelParameterCollection)]],
    protected override val currentResults: TxnVar[F, Queue[(Int, Double)]],
    protected override val currentBest: TxnVar[F, (Int, Double)],
    protected override val currentSimplex: TxnVar[F, ModelParameterSimplex],
    protected override val isConverged: TxnVar[F, Boolean]
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
      iterationUpdateCallback: MdsTelemetryUpdate => F[Unit],
      isConvergedCallback: Unit => F[Unit]
  ): F[MdsOptimisedPosterior[F]] =
    for {
      parallelismTokenPool <- TxnVar.of(mdsConfig.evaluationParallelism)
      queuedEvaluations    <- TxnVar.of(Queue[(Int, ModelParameterCollection)]())
      currentResults       <- TxnVar.of(Queue[(Int, Double)]())
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
      currentBest = currentBest,
      currentSimplex = currentSimplex,
      isConverged = isConverged
    )
}
