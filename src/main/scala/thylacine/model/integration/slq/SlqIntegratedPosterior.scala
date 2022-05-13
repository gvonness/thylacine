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
package thylacine.model.integration.slq

import thylacine.config.SlqConfig
import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.interface.SlqTelemetryUpdate

case class SlqIntegratedPosterior(
    slqConfig: SlqConfig,
    posterior: Posterior[Prior[_], _],
    seeds: Set[ModelParameterCollection] = Set(),
    slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit = _ => (),
    domainRebuildStartCallback: Unit => Unit = _ => (),
    domainRebuildFinishCallback: Unit => Unit = _ => ()
) extends SlqEngine {
  override protected final val slqSamplePoolSize: Int = slqConfig.poolSize

  override protected final val slqNumberOfAbscissa: Int =
    slqConfig.abscissaNumber

  override protected final val slqScalingIncrement: Double =
    slqConfig.domainScalingIncrement

  override protected final val slqNominalAcceptanceRatio: Double =
    slqConfig.targetAcceptanceProbability

  override protected final val slqSampleParallelism: Int =
    slqConfig.sampleParallelism

}
