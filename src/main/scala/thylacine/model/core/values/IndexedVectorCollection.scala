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

import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.{CanValidate, GenericIdentifier}

private[thylacine] case class IndexedVectorCollection(
    index: Map[ModelParameterIdentifier, VectorContainer],
    validated: Boolean = false
) extends IndexedCollection[VectorContainer]
    with CanValidate[IndexedVectorCollection] {

  private[thylacine] override lazy val getValidated: IndexedVectorCollection =
    if (validated) {
      this
    } else {
      IndexedVectorCollection(
        index.map(i => i._1 -> i._2.getValidated),
        validated = true
      )
    }

  private[thylacine] lazy val genericScalaRepresentation: Map[String, Vector[Double]] =
    index.map(i => i._1.value -> i._2.scalaVector)

  private[thylacine] lazy val magnitude =
    Math.sqrt(index.values.map(_.squaredMagnitude).sum)

  private[thylacine] lazy val totalDimension =
    index.values.map(_.dimension).sum

  // Low-level API
  // -------------
  private[thylacine] def rawMergeWith(
      other: IndexedVectorCollection
  ): IndexedVectorCollection = {
    val keyIntersection = index.keySet.intersect(other.index.keySet)
    if (keyIntersection.nonEmpty) {
      throw new RuntimeException(
        s"Can not merge indexed vector collections with the same labels: $keyIntersection"
      )
    } else {
      IndexedVectorCollection(getValidated.index ++ other.getValidated.index, validated = true)
    }
  }

  private[thylacine] def rawSumWith(
      other: IndexedVectorCollection
  ): IndexedVectorCollection = {
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

  private[thylacine] def rawScalarMultiplyWith(
      input: Double
  ): IndexedVectorCollection =
    this.copy(
      index = index.view.mapValues(_.rawScalarProductWith(input)).toMap
    )

  private[thylacine] def rawSubtract(
      other: IndexedVectorCollection
  ): IndexedVectorCollection =
    rawSumWith(other.rawScalarMultiplyWith(-1.0))

  private[thylacine] def rawNudgeComponents(
      diff: Double
  ): Map[ModelParameterIdentifier, List[IndexedVectorCollection]] = {
    val differentials: Map[GenericIdentifier, List[VectorContainer]] =
      index.map { i =>
        i._1 -> i._2.rawNudgeComponents(diff)
      }

    index.collect { k =>
      differentials.get(k._1) match {
        case Some(vs) =>
          k._1 -> vs.map(v => IndexedVectorCollection(index + (k._1 -> v)))
      }
    }
  }

  private[thylacine] def distanceTo(other: IndexedVectorCollection): Double =
    this.rawSubtract(other).magnitude
}

private[thylacine] object IndexedVectorCollection {

  // A particular type of this collection is of the model
  // parameters directly.
  private[thylacine] type ModelParameterCollection = IndexedVectorCollection

  private[thylacine] val empty: IndexedVectorCollection =
    IndexedVectorCollection(
      index = Map(),
      validated = true
    )

  private[thylacine] def apply(
      identifier: ModelParameterIdentifier,
      vector: VectorContainer
  ): IndexedVectorCollection =
    IndexedVectorCollection(
      index = Map(identifier -> vector.getValidated),
      validated = true
    )

  private[thylacine] def apply(
      identifierLabel: String,
      values: Vector[Double]
  ): IndexedVectorCollection =
    apply(ModelParameterIdentifier(identifierLabel), VectorContainer(values))

  private[thylacine] def apply(
      labeledLists: Map[String, Vector[Double]]
  ): IndexedVectorCollection =
    if (labeledLists.nonEmpty)
      labeledLists.map(i => apply(i._1, i._2)).reduce(_ rawMergeWith _)
    else
      empty

  private[thylacine] def merge(
      modelParameters: Seq[IndexedVectorCollection]
  ): IndexedVectorCollection =
    modelParameters.reduce(_ rawMergeWith _)
}
