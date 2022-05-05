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

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._

case class IndexedVectorCollection(
    index: Map[ModelParameterIdentifier, VectorContainer],
    validated: Boolean = false
) extends IndexedCollection[VectorContainer]
    with CanValidate[IndexedVectorCollection] {

  override lazy val getValidated: IndexedVectorCollection =
    if (validated) {
      this
    } else {
      IndexedVectorCollection(
        index.map(i => i._1 -> i._2.getValidated),
        validated = true
      )
    }

  // Low-level API
  // -------------
  def rawMergeWith(
      other: IndexedVectorCollection
  ): IndexedVectorCollection = {
    val keyIntersection = index.keySet.intersect(other.index.keySet)
    if (keyIntersection.nonEmpty) {
      throw new RuntimeException(
        s"Can not merge indexed vector collections with the same labels: $keyIntersection"
      )
    } else {
      IndexedVectorCollection(getValidated.index ++ other.getValidated.index,
                              validated = true
      )
    }
  }

  def rawSumWith(other: IndexedVectorCollection): IndexedVectorCollection = {
    val keySet = getValidated.index.keySet ++ other.getValidated.index.keySet

    val newIndex =
      keySet.flatMap { k =>
        (getValidated.index.get(k), other.getValidated.index.get(k)) match {
          case (Some(v1), Some(v2)) =>
            Some(k -> (v1 rawSumWith v2))
          case (Some(v1), _) =>
            Some(k -> v1)
          case (_, Some(v2)) =>
            Some(k -> v2)
          case _ =>
            None
        }
      }

    IndexedVectorCollection(index = newIndex.toMap, validated = true)
  }

  def rawScalarMultiplyWith(input: Double): IndexedVectorCollection =
    this.copy(
      index = index.view.mapValues(_.rawScalarProductWith(input)).toMap
    )

  def rawSubtract(other: IndexedVectorCollection): IndexedVectorCollection =
    rawSumWith(other.rawScalarMultiplyWith(-1.0))
}

object IndexedVectorCollection {

  // A particular type of this collection is of the model
  // parameters directly.
  type ModelParameterCollection = IndexedVectorCollection

  val empty: IndexedVectorCollection =
    IndexedVectorCollection(
      index = Map(),
      validated = true
    )

  def apply(
      identifier: ModelParameterIdentifier,
      vector: VectorContainer
  ): IndexedVectorCollection =
    IndexedVectorCollection(
      index = Map(identifier -> vector.getValidated),
      validated = true
    )

  def merge(
      modelParameters: Seq[IndexedVectorCollection]
  ): ResultOrErrIo[IndexedVectorCollection] =
    ResultOrErrIo.fromCalculation(modelParameters.reduce(_ rawMergeWith _))
}
