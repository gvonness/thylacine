/*
 * Copyright 2023 Greg von Nessi
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
package thylacine.model.core.values.modelparameters

import thylacine.model.core.GenericIdentifier.ModelParameterIdentifier
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{ IndexedVectorCollection, VectorContainer }

import breeze.linalg.DenseVector

import scala.annotation.unused

private[thylacine] trait ModelParameterContext {
  private[thylacine] def orderedParameterIdentifiersWithDimension: Vector[(ModelParameterIdentifier, Int)]

  final private[thylacine] def zeroModelParameterCollection: ModelParameterCollection =
    IndexedVectorCollection(
      orderedParameterIdentifiersWithDimension.toMap.view
        .mapValues(VectorContainer.zeros)
        .toMap
    )

  @unused
  final def zeroParameterMapping: Map[String, Vector[Double]] =
    zeroModelParameterCollection.genericScalaRepresentation

  final private[thylacine] def rawVectorToModelParameterCollection(
    input: DenseVector[Double]
  ): ModelParameterCollection =
    vectorValuesToModelParameterCollection(input.toArray.toVector)

  final private[thylacine] def vectorValuesToModelParameterCollection(
    input: Vector[Double]
  ): ModelParameterCollection =
    orderedParameterIdentifiersWithDimension
      .foldLeft(
        (input, IndexedVectorCollection.empty)
      ) { (i, j) =>
        val (vector, remainder) = i._1.splitAt(j._2)

        (
          remainder,
          i._2.rawMergeWith(
            IndexedVectorCollection(j._1, VectorContainer(vector))
          )
        )
      }
      ._2

  final private[thylacine] def modelParameterCollectionToVectorValues(
    input: ModelParameterCollection
  ): Vector[Double] =
    orderedParameterIdentifiersWithDimension
      .foldLeft(Vector[Vector[Double]]()) { case (current, (identifier, _)) =>
        input.retrieveIndex(identifier).scalaVector +: current
      }
      .reverse
      .reduce(_ ++ _)

  final private[thylacine] def modelParameterCollectionToRawVector(
    input: ModelParameterCollection
  ): DenseVector[Double] =
    DenseVector {
      modelParameterCollectionToVectorValues(input).toArray
    }
}
