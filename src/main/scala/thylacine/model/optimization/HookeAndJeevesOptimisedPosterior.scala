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
package thylacine.model.optimization

import thylacine.config.HookeAndJeevesConfig
import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior

import ai.entrolution.bengal.stm.STM
import cats.effect.IO

case class HookeAndJeevesOptimisedPosterior(
    hookeAndJeevesConfig: HookeAndJeevesConfig,
    posterior: Posterior[Prior[_], _],
    newMaximumCallback: Double => Unit = _ => (),
    newScaleCallback: Double => Unit = _ => (),
    isConvergedCallback: Unit => Unit = _ => ()
)(implicit stm: STM[IO])
    extends HookeAndJeevesEngine {

  override protected val convergenceThreshold: Double =
    hookeAndJeevesConfig.convergenceThreshold

  override protected val numberOfSamplesToSetScale: Int =
    hookeAndJeevesConfig.numberOfPriorSamplesToSetScale.getOrElse(100)
}
