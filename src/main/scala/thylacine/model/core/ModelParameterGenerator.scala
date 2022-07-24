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
package thylacine.model.core

import thylacine.model.core.GenericIdentifier._

private[thylacine] trait ModelParameterGenerator {
  // Model parameter dimensions need to be disjoint and uniquely
  // identifiable. We could enforce this via typing, but it's
  // preferred to relegate typing to enforce the Bayesian model
  // structure and not get entangled with the underpinning calculations
  private[thylacine] def identifier: ModelParameterIdentifier

  private[thylacine] def generatorDimension: Int
}
