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

import thylacine.config.HmcmcConfig
import thylacine.model.components.posterior.NonAnalyticPosterior
import thylacine.model.core.Erratum.ResultOrErrIo
import thylacine.model.core.IndexedVectorCollection
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection

import cats.effect.IO

case class HmcmcPosteriorSampler(
    hmcmcConfig: HmcmcConfig,
    posterior: NonAnalyticPosterior,
    sampleRequestSetCallback: Int => Unit,
    sampleRequestUpdateCallback: Int => Unit,
    seedSpec: IO[Map[String, Vector[Double]]] = IO.pure(Map())
) extends HmcmcEngine {

  override protected final val simulationsBetweenSamples: Int =
    hmcmcConfig.stepsBetweenSamples

  override protected final val stepsInSimulation: Int =
    hmcmcConfig.stepsInDynamicsSimulation

  override protected final val simulationEpsilon: Double =
    hmcmcConfig.dynamicsSimulationStepSize

  override protected final val warmUpSimulationCount: Int =
    hmcmcConfig.warmupStepCount

  override protected final val startingPoint: ResultOrErrIo[ModelParameterCollection] =
    ResultOrErrIo.fromIo(seedSpec.map(IndexedVectorCollection(_)))

  def init: IO[Unit] =
    (for {
      pt <- startingPoint
      _ <- if (pt.index.isEmpty) {
             ResultOrErrIo.fromIo(initialise)
           } else {
             ResultOrErrIo.fromIo(initialise(pt))
           }
      _ <- ResultOrErrIo.fromIo(waitForInitialisation)
    } yield ()).value.void
}
