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

import thylacine.config.SlqConfig
import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._
import thylacine.model.integration.slq.SlqEngine
import thylacine.model.support.SlqTelemetryUpdate

case class SlqIntegratedPosterior(
    slqConfig: SlqConfig,
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]],
    seeds: Set[ModelParameterCollection] = Set(),
    slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit
) extends NonAnalyticPosterior(priors, likelihoods)
    with SlqEngine[Prior[_], Likelihood[_, _]] {
  override protected final val slqSamplePoolSize: Int = slqConfig.poolSize

  override protected final val slqNumberOfAbscissa: Int =
    slqConfig.abscissaNumber

  override protected final val slqScalingIncrement: Double =
    slqConfig.domainScalingIncrement

  override protected final val slqNominalAcceptanceRatio: Double =
    slqConfig.targetAcceptanceProbability

  override protected final val slqSampleParallelism: Int =
    slqConfig.sampleParallelism

  override def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    getSimulatedSample

  override protected def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)
}
