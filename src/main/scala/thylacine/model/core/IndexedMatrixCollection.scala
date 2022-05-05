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

import thylacine.model.core.GenericIdentifier.ModelParameterIdentifier

case class IndexedMatrixCollection(
    index: Map[ModelParameterIdentifier, MatrixContainer],
    validated: Boolean = false
) extends IndexedCollection[MatrixContainer]
    with CanValidate[IndexedMatrixCollection] {

  override lazy val getValidated: IndexedMatrixCollection =
    if (validated) {
      this
    } else {
      IndexedMatrixCollection(
        index.map(i => i._1 -> i._2.getValidated),
        validated = true
      )
    }
}

object IndexedMatrixCollection {

  def apply(
      identifier: ModelParameterIdentifier,
      values: List[List[Double]]
  ): IndexedMatrixCollection =
    IndexedMatrixCollection(
      index = Map(identifier -> MatrixContainer(values))
    )
}
