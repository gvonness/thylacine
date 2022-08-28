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
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.computation.{CachedComputation, ResultOrErrF}
import thylacine.model.core.values._

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.annotation.unused

// A linear forward model may work across more than
// one model parameter generator
case class LinearForwardModel[F[_]: STM: Async](
    override val evalCache: CachedComputation[F, VectorContainer],
    override val jacobianCache: CachedComputation[F, IndexedMatrixCollection[F]],
    transform: IndexedMatrixCollection[F],
    vectorOffset: Option[VectorContainer],
    override val domainDimension: Int,
    override val rangeDimension: Int,
    override val validated: Boolean = false
) extends StmImplicits[F]
    with InMemoryMemoizedForwardModel[F] {
  if (!validated) {
    assert(transform.index.map(_._2.rowTotalNumber).toSet.size == 1)
    assert(
      vectorOffset.forall(vo => vo.dimension == transform.index.head._2.rowTotalNumber)
    )
  }

  private[thylacine] override lazy val getValidated: LinearForwardModel[F] =
    if (validated) {
      this
    } else {
      this.copy(
        transform = transform.getValidated,
        vectorOffset = vectorOffset.map(_.getValidated),
        validated = true
      )
    }

  // Convenience method, as the Jacobian for linear models is obviously
  // constant
  private[thylacine] val getJacobian: ResultOrErrF[F, IndexedMatrixCollection[F]] =
    transform.toResultM

  override protected def jacobianAt(
      input: IndexedVectorCollection[F]
  ): ResultOrErrF[F, IndexedMatrixCollection[F]] =
    getJacobian
}

object LinearForwardModel {

  def of[F[_]: STM: Async](
      transform: IndexedMatrixCollection[F],
      vectorOffset: Option[VectorContainer],
      evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] = {
    val rangeDimension: Int =
      transform.index.head._2.rowTotalNumber

    val domainDimension: Int =
      transform.index.map(_._2.columnTotalNumber).sum

    val orderedParameterIdentifiersWithDimension: ResultOrErrF[F, Vector[(ModelParameterIdentifier, Int)]] =
      transform.index
        .map(i => (i._1, i._2.columnTotalNumber))
        .toVector
        .sortBy(_._1.value)
        .toResultM

    val rawMatrixTransform: ResultOrErrF[F, DenseMatrix[Double]] =
      for {
        identifiersAndDimensions <- orderedParameterIdentifiersWithDimension
        result <- identifiersAndDimensions
                    .map(_._1)
                    .foldLeft(
                      MatrixContainer.zeros(rowDimension = rangeDimension, columnDimension = 0).toResultM
                    ) { (i, j) =>
                      for {
                        prev            <- i
                        matrixContainer <- transform.retrieveIndex(j)
                      } yield prev.columnMergeWith(matrixContainer)
                    }
      } yield result.rawMatrix

    def applyOffset(input: DenseVector[Double]): DenseVector[Double] =
      vectorOffset.map(_.rawVector + input).getOrElse(input)

    def modelParameterCollectionToRawVector(value: IndexedVectorCollection[F]): ResultOrErrF[F, DenseVector[Double]] =
      for {
        identifiersAndDimensions <- orderedParameterIdentifiersWithDimension
        result <- identifiersAndDimensions
                    .map(_._1)
                    .foldLeft(
                      VectorContainer.zeros(domainDimension).toResultM
                    ) { (i, j) =>
                      for {
                        prev            <- i
                        vectorContainer <- value.retrieveIndex(j)
                      } yield prev.rawConcatenateWith(vectorContainer)
                    }
      } yield result.rawVector

    def transformedEval(
        input: IndexedVectorCollection[F]
    ): ResultOrErrF[F, VectorContainer] =
      for {
        rawVector    <- modelParameterCollectionToRawVector(input)
        rawTransform <- rawMatrixTransform
      } yield VectorContainer(applyOffset(rawTransform * rawVector))

    def dummyMapping(@unused input: IndexedVectorCollection[F]): ResultOrErrF[F, IndexedMatrixCollection[F]] =
      IndexedMatrixCollection(index = Map()).toResultM

    for {
      evalCache <- CachedComputation.of(transformedEval, evalCacheDepth)
      jacobianCache <-
        CachedComputation.of(dummyMapping, None)
    } yield LinearForwardModel[F](evalCache, jacobianCache, transform, vectorOffset, domainDimension, rangeDimension)
  }

  private[thylacine] def of[F[_]: STM: Async](
      identifier: ModelParameterIdentifier,
      values: Vector[Vector[Double]],
      evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] =
    of[F](
      transform = IndexedMatrixCollection(identifier, values),
      vectorOffset = None,
      evalCacheDepth = evalCacheDepth
    )

  def of[F[_]: STM: Async](
      label: String,
      values: Vector[Vector[Double]],
      evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] =
    of[F](
      transform = IndexedMatrixCollection(ModelParameterIdentifier(label), values),
      vectorOffset = None,
      evalCacheDepth = evalCacheDepth
    )

  def identityOf[F[_]: STM: Async](
      label: String,
      dimension: Int,
      evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] =
    of[F](
      transform = IndexedMatrixCollection
        .squareIdentity(ModelParameterIdentifier(label), dimension),
      vectorOffset = None,
      evalCacheDepth = evalCacheDepth
    )
}
