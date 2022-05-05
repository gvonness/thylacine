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

/** We use the type system to ensure the right structure
  * of the Bayesian Graph components. but do not use it in
  * validating the mathematics of the underpinning
  * computations. Doing so would make the typing model
  * significantly more complicated to get some relative
  * simple sanity checks in place.
  *
  * Instead we create a flexible validation framework that
  * will be enacted at the creation of any Bayesian graphical
  * network. Obviously, this won't get us compile time
  * guarantees, but violations will fail the model quickly
  * at runtime
  */
private[thylacine] trait CanValidate[+T <: CanValidate[_]] {
  private[thylacine] def validated: Boolean
  private[thylacine] def getValidated: T
}
