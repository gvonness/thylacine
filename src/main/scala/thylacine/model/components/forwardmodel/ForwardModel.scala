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
package thylacine.model.components.forwardmodel

import thylacine.model.core._
import thylacine.model.core.Erratum._

private[thylacine] trait ForwardModel
    extends GenericMapping
    with ModelParameterRawMappings
    with CanValidate[ForwardModel] {

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def evalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer]

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def jacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection]
}
