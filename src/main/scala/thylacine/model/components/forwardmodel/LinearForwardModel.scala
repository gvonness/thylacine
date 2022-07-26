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

import bengal.stm.STM
import thylacine.model.components.forwardmodel.InMemoryMemoizedForwardModel.ForwardModelCachingConfig
import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.effect.IO

// A linear forward model may work across more than
// one model parameter generator
case class LinearForwardModel(
    transform: IndexedMatrixCollection,
    vectorOffset: Option[VectorContainer],
    maxResultsToCache: Int,
    override val validated: Boolean = false
)(implicit stm: STM[IO])
    extends InMemoryMemoizedForwardModel {
  if (!validated) {
    assert(transform.index.map(_._2.rowTotalNumber).toSet.size == 1)
    assert(
      vectorOffset.forall(vo => vo.dimension == transform.index.head._2.rowTotalNumber)
    )
  }

  override protected val cacheConfig: ForwardModelCachingConfig =
    ForwardModelCachingConfig(evalCacheDepth = Some(maxResultsToCache), jacobianCacheDepth = None)

  private[thylacine] override lazy val getValidated: LinearForwardModel =
    if (validated) {
      this
    } else {
      LinearForwardModel(transform.getValidated, vectorOffset.map(_.getValidated), maxResultsToCache, validated = true)
    }

  override protected val orderedParameterIdentifiersWithDimension
      : ResultOrErrIo[Vector[(ModelParameterIdentifier, Int)]] =
    ResultOrErrIo.fromCalculation(
      transform.index
        .map(i => (i._1, i._2.columnTotalNumber))
        .toVector
        .sortBy(_._1.value)
    )

  override val rangeDimension: Int =
    transform.index.head._2.rowTotalNumber

  override val domainDimension: Int =
    transform.index.map(_._2.columnTotalNumber).sum

  val rawMatrixTransform: ResultOrErrIo[DenseMatrix[Double]] =
    for {
      identifiersAndDimensions <- orderedParameterIdentifiersWithDimension
      result <- identifiersAndDimensions
                  .map(_._1)
                  .foldLeft(
                    ResultOrErrIo.fromValue(
                      MatrixContainer.zeros(rowDimension = rangeDimension, columnDimension = 0)
                    )
                  ) { (i, j) =>
                    for {
                      prev            <- i
                      matrixContainer <- transform.retrieveIndex(j)
                    } yield prev.columnMergeWith(matrixContainer)
                  }
    } yield result.rawMatrix

  private def applyOffset(input: DenseVector[Double]): DenseVector[Double] =
    vectorOffset.map(_.rawVector + input).getOrElse(input)

  override protected def computeEvalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer] =
    for {
      rawVector    <- modelParameterCollectionToRawVector(input)
      rawTransform <- rawMatrixTransform
    } yield VectorContainer(applyOffset(rawTransform * rawVector))

  // Convenience method, as the Jacobian for linear models is obviously
  // constant
  private[thylacine] val getJacobian: ResultOrErrIo[IndexedMatrixCollection] =
    ResultOrErrIo.fromValue(transform)

  override protected def computeJacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection] =
    getJacobian
}

object LinearForwardModel {

  private[thylacine] def apply(
      identifier: ModelParameterIdentifier,
      values: Vector[Vector[Double]],
      maxResultsToCache: Int
  )(implicit stm: STM[IO]): LinearForwardModel =
    LinearForwardModel(
      transform = IndexedMatrixCollection(identifier, values),
      vectorOffset = None,
      maxResultsToCache = maxResultsToCache
    )

  def apply(
      label: String,
      values: Vector[Vector[Double]],
      maxResultsToCache: Int
  )(implicit stm: STM[IO]): LinearForwardModel =
    LinearForwardModel(
      transform = IndexedMatrixCollection(label, values),
      vectorOffset = None,
      maxResultsToCache = maxResultsToCache
    )
}
