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

import thylacine.model.core.Erratum.{ResultOrErrF, ResultOrErrIo}
import thylacine.model.core.GenericIdentifier.ModelParameterIdentifier
import thylacine.model.core.{IndexedMatrixCollection, IndexedVectorCollection, VectorContainer}

import ai.entrolution.bengal.stm.STM
import ai.entrolution.thylacine.model.components.forwardmodel.InMemoryMemoizedForwardModel.ForwardModelCachingConfig
import cats.effect.IO

case class NonLinearInMemoryMemoizedForwardModel(
    evaluation: Map[String, Vector[Double]] => Vector[Double],
    jacobian: Map[String, Vector[Double]] => Map[String, Vector[
      Vector[Double]
    ]],
    domainDimensions: Map[String, Int],
    override val rangeDimension: Int,
    maxResultsToCache: Int,
    override val validated: Boolean = false
)(implicit stm: STM[IO])
    extends InMemoryMemoizedForwardModel {

  override protected val cacheConfig: ForwardModelCachingConfig =
    ForwardModelCachingConfig(evalCacheDepth = Some(maxResultsToCache), jacobianCacheDepth = Some(maxResultsToCache))

  override protected val orderedParameterIdentifiersWithDimension
      : ResultOrErrIo[Vector[(ModelParameterIdentifier, Int)]] =
    ResultOrErrIo.fromCalculation(
      domainDimensions.map(i => (ModelParameterIdentifier(i._1), i._2)).toVector
    )

  override private[thylacine] val getValidated = this

  override val domainDimension: Int = domainDimensions.values.sum

  override protected def computeEvalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation {
      VectorContainer(
        evaluation(
          input.index.map(i => i._1.value -> i._2.scalaVector)
        )
      )
    }

  override protected def computeJacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection] =
    ResultOrErrIo.fromCalculation {
      jacobian(
        input.index.map(i => i._1.value -> i._2.scalaVector)
      ).map { case (label, matrixValue) =>
        IndexedMatrixCollection(ModelParameterIdentifier(label), matrixValue)
      }.reduce(_ rawMergeWith _)
    }
}
