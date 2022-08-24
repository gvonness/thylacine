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
package thylacine.model.core.values

import thylacine.model.core.CanValidate
import thylacine.model.core.GenericIdentifier.ModelParameterIdentifier

import cats.effect.kernel.Async

private[thylacine] case class IndexedMatrixCollection[F[_] : Async](
    index: Map[ModelParameterIdentifier, MatrixContainer],
    validated: Boolean = false
) extends IndexedCollection[F, MatrixContainer]
    with CanValidate[IndexedMatrixCollection[F]] {

  private[thylacine] override lazy val getValidated: IndexedMatrixCollection[F] =
    if (validated) {
      this
    } else {
      IndexedMatrixCollection(
        index.map(i => i._1 -> i._2.getValidated),
        validated = true
      )
    }

  private[thylacine] lazy val genericScalaRepresentation: Map[String, Vector[Vector[Double]]] =
    index.map(i => i._1.value -> i._2.genericScalaRepresentation)

  private[thylacine] def rawMergeWith(
      other: IndexedMatrixCollection
  ): IndexedMatrixCollection =
    IndexedMatrixCollection(index ++ other.index).getValidated
}

private[thylacine] object IndexedMatrixCollection {

  private[thylacine] def apply(
      identifier: ModelParameterIdentifier,
      values: Vector[Vector[Double]]
  ): IndexedMatrixCollection =
    IndexedMatrixCollection(
      index = Map(identifier -> MatrixContainer(values))
    )

  private[thylacine] def squareIdentity(
      identifier: ModelParameterIdentifier,
      dimension: Int
  ): IndexedMatrixCollection =
    IndexedMatrixCollection(
      index = Map(identifier -> MatrixContainer.squareIdentity(dimension))
    )
}
