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

import thylacine.model.core.Erratum.ResultOrErrF
import thylacine.model.core.Erratum.ResultOrErrF.Implicits._
import thylacine.model.core.GenericIdentifier.ModelParameterIdentifier
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection

import breeze.linalg.DenseVector

private[thylacine] trait ModelParameterRawMappings {

  protected def orderedParameterIdentifiersWithDimension[F[_]]: ResultOrErrF[F, Vector[(ModelParameterIdentifier, Int)]]

  private[thylacine] final def rawVectorToModelParameterCollection[F[_]](
      input: DenseVector[Double]
  ): ResultOrErrF[F, ModelParameterCollection] =
    vectorValuesToModelParameterCollection(input.toArray.toVector)

  private[thylacine] final def vectorValuesToModelParameterCollection[F[_]](
      input: Vector[Double]
  ): ResultOrErrF[F, ModelParameterCollection] =
    orderedParameterIdentifiersWithDimension.map {
      _.foldLeft(
        (input, IndexedVectorCollection.empty)
      ) { (i, j) =>
        val (vector, remainder) = i._1.splitAt(j._2)

        (remainder,
         i._2.rawMergeWith(
           IndexedVectorCollection(j._1, VectorContainer(vector))
         )
        )
      }
    }.map(_._2)

  private[thylacine] final def modelParameterCollectionToRawVector[F[_]](
      input: ModelParameterCollection
  ): ResultOrErrF[F, DenseVector[Double]] =
    orderedParameterIdentifiersWithDimension.flatMap { op =>
      op.foldLeft(Vector[Vector[Double]]().toResultM) { (i, j) =>
        for {
          current  <- i
          toAppend <- input.retrieveIndex(j._1)
        } yield toAppend.scalaVector +: current
      }.map(i => DenseVector(i.reverse.reduce(_ ++ _).toArray))
    }

  private[thylacine] final def modelParameterCollectionToVectorValues[F[_]](
      input: ModelParameterCollection
  ): ResultOrErrF[F, Vector[Double]] =
    modelParameterCollectionToRawVector(input).map(_.toArray.toVector)
}
