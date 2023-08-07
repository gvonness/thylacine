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

import thylacine.config.HmcmcConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.HmcmcTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{ IndexedVectorCollection, VectorContainer }
import thylacine.model.sampling.hmcmc.HmcmcEngine

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.annotation.unused

case class HmcmcSampledPosterior[F[_]: Async](
  private[thylacine] val hmcmcConfig: HmcmcConfig,
  override protected val telemetryUpdateCallback: HmcmcTelemetryUpdate => F[Unit],
  private[thylacine] val seed: Map[String, Vector[Double]],
  override private[thylacine] val priors: Set[Prior[F, _]],
  override private[thylacine] val likelihoods: Set[Likelihood[F, _, _]]
) extends AsyncImplicits[F]
    with Posterior[F, Prior[F, _], Likelihood[F, _, _]]
    with HmcmcEngine[F] {

  final override protected val simulationsBetweenSamples: Int =
    hmcmcConfig.stepsBetweenSamples

  final override protected val stepsInSimulation: Int =
    hmcmcConfig.stepsInDynamicsSimulation

  final override protected val simulationEpsilon: Double =
    hmcmcConfig.dynamicsSimulationStepSize

  final override protected val warmUpSimulationCount: Int =
    hmcmcConfig.warmupStepCount

  final override protected val startingPoint: F[ModelParameterCollection] =
    Async[F].delay(IndexedVectorCollection(seed))

  final override protected def rawSampleModelParameters: F[VectorContainer] =
    sampleModelParameters(1).map(s => VectorContainer(modelParameterCollectionToRawVector(s.head)))
}

@unused
object HmcmcSampledPosterior {

  @unused
  def from[F[_]: Async](
    hmcmcConfig: HmcmcConfig,
    posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
    telemetryUpdateCallback: HmcmcTelemetryUpdate => F[Unit],
    seed: Map[String, Vector[Double]]
  ): HmcmcSampledPosterior[F] =
    HmcmcSampledPosterior(
      hmcmcConfig             = hmcmcConfig,
      telemetryUpdateCallback = telemetryUpdateCallback,
      seed                    = seed,
      priors                  = posterior.priors,
      likelihoods             = posterior.likelihoods
    )

}
