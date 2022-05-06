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
import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum.ResultOrErrIo
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._
import thylacine.model.sampling.HmcmcEngine

case class HmcmcSampledPosterior(
    hmcmcConfig: HmcmcConfig,
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]],
    sampleRequestSetCallback: Int => Unit,
    sampleRequestUpdateCallback: Int => Unit
) extends NonAnalyticPosterior(priors, likelihoods)
    with HmcmcEngine[Prior[_], Likelihood[_, _]] {

  override protected final val simulationsBetweenSamples: Int =
    hmcmcConfig.stepsBetweenSamples

  override protected final val stepsInSimulation: Int =
    hmcmcConfig.stepsInDynamicsSimulation

  override protected final val simulationEpsilon: Double =
    hmcmcConfig.dynamicsSimulationStepSize

  override protected final val warmUpSimulationCount: Int =
    hmcmcConfig.warmupStepCount

  override def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    getHmcmcSample

  override protected def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)
}
