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
package thylacine.model.sampling.hmcmc

import bengal.stm.STM
import thylacine.config.HmcmcConfig
import thylacine.model.components.posterior.NonAnalyticPosterior
import thylacine.model.core.Erratum.{ResultOrErrF, ResultOrErrIo}
import thylacine.model.core.IndexedVectorCollection
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection

import cats.effect.IO

case class HmcmcPosteriorSampler(
    hmcmcConfig: HmcmcConfig,
    posterior: NonAnalyticPosterior,
    sampleRequestSetCallback: Int => Unit = _ => (),
    sampleRequestUpdateCallback: Int => Unit = _ => (),
    epsilonUpdateCallback: Double => Unit = _ => (),
    dhMonitorCallback: Double => Unit = _ => (),
    seedSpec: IO[Map[String, Vector[Double]]] = IO.pure(Map())
)(implicit stm: STM[IO])
    extends HmcmcEngine {

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

  override protected final val startingPoint: ResultOrErrIo[ModelParameterCollection] =
    ResultOrErrIo.fromIo(seedSpec.map(IndexedVectorCollection(_)))

  def init: IO[Unit] =
    for {
      _ <- initialise
      _ <- waitForInitialisation
    } yield ()
}
