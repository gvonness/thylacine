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
package thylacine.model.components.posterior

import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

import breeze.linalg.DenseVector

trait Posterior[P <: Prior[_], L <: Likelihood[_, _]]
    extends ModelParameterPdf
    with ModelParameterSampleGenerator {
  def priors: Set[P]
  def likelihoods: Set[L]
  def isAnalytic: Boolean

  override final val domainDimension = priors.toList.map(_.domainDimension).sum

  final protected lazy val orderedParameterIdentifiersWithDimension
      : ResultOrErrIo[List[(ModelParameterIdentifier, Int)]] =
    ResultOrErrIo.fromCalculation {
      priors.toList
        .sortBy(_.posteriorTermIdentifier)
        .map(i =>
          ModelParameterIdentifier(
            i.posteriorTermIdentifier.value
          ) -> i.generatorDimension
        )
    }

  final protected def rawVectorToModelParameterCollection(
      input: DenseVector[Double]
  ): ResultOrErrIo[ModelParameterCollection] =
    listValuesToModelParameterCollection(input.toArray.toList)

  final protected def listValuesToModelParameterCollection(
      input: List[Double]
  ): ResultOrErrIo[ModelParameterCollection] =
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

  final protected def modelParameterCollectionToRawVector(
      input: ModelParameterCollection
  ): ResultOrErrIo[DenseVector[Double]] =
    orderedParameterIdentifiersWithDimension.flatMap { op =>
      op.foldLeft(ResultOrErrIo.fromValue(List[List[Double]]())) { (i, j) =>
        for {
          current  <- i
          toAppend <- input.retrieveIndex(j._1)
        } yield toAppend.rawVector.toScalaVector().toList :: current
      }.map(i => DenseVector(i.reverse.reduce(_ ::: _).toArray))
    }

  final protected def modelParameterCollectionToListValues(
      input: ModelParameterCollection
  ): ResultOrErrIo[List[Double]] =
    modelParameterCollectionToRawVector(input).map(_.toArray.toList)
}
