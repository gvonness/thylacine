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
package thylacine.model.components.forwardmodel

import bengal.stm.STM
import thylacine.model.core.GenericIdentifier.*
import thylacine.model.core.*
import thylacine.model.core.computation.CachedComputation
import thylacine.model.core.values.*
import thylacine.model.core.values.modelparameters.ModelParameterContext

import breeze.linalg.{ DenseMatrix, DenseVector }
import cats.effect.kernel.Async
import cats.syntax.all.*

import scala.annotation.unused

// A linear forward model may work across more than
// one model parameter generator
case class LinearForwardModel[F[_]: STM: Async](
  override protected val evalCache: CachedComputation[F, VectorContainer],
  override protected val jacobianCache: CachedComputation[F, IndexedMatrixCollection],
  private[thylacine] val transform: IndexedMatrixCollection,
  private[thylacine] val vectorOffset: Option[VectorContainer],
  override val domainDimension: Int,
  override val rangeDimension: Int,
  override private[thylacine] val validated: Boolean = false
) extends StmImplicits[F]
    with InMemoryMemoizedForwardModel[F] {
  if (!validated) {
    assert(transform.index.map(_._2.rowTotalNumber).toSet.size == 1)
    assert(
      vectorOffset.forall(vo => vo.dimension == transform.index.head._2.rowTotalNumber)
    )
  }

  override private[thylacine] lazy val getValidated: LinearForwardModel[F] =
    if (validated) {
      this
    } else {
      this.copy(
        transform    = transform.getValidated,
        vectorOffset = vectorOffset.map(_.getValidated),
        validated    = true
      )
    }

  // Convenience method, as the Jacobian for linear models is obviously
  // constant
  private[thylacine] val getJacobian: IndexedMatrixCollection =
    transform

  override private[thylacine] def jacobianAt(
    input: IndexedVectorCollection
  ): F[IndexedMatrixCollection] =
    Async[F].pure(getJacobian)
}

object LinearForwardModel {

  def of[F[_]: STM: Async](
    transform: IndexedMatrixCollection,
    vectorOffset: Option[VectorContainer],
    evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] = {
    val rangeDimension: Int =
      transform.index.head._2.rowTotalNumber

    val domainDimension: Int =
      transform.index.map(_._2.columnTotalNumber).sum

    val orderedLabelsAndDimensions: Vector[(ModelParameterIdentifier, Int)] =
      transform.index
        .map(i => (i._1, i._2.columnTotalNumber))
        .toVector
        .sortBy(_._1.value)

    val rawMatrixTransform: DenseMatrix[Double] =
      orderedLabelsAndDimensions
        .map(_._1)
        .foldLeft(
          MatrixContainer.zeros(rowDimension = rangeDimension, columnDimension = 0)
        ) { case (matrixContainer, identifier) =>
          matrixContainer.columnMergeWith(transform.retrieveIndex(identifier))
        }
        .rawMatrix

    def applyOffset(input: DenseVector[Double]): DenseVector[Double] =
      vectorOffset.map(_.rawVector + input).getOrElse(input)

    val rawMappings = new ModelParameterContext {
      override private[thylacine] val orderedParameterIdentifiersWithDimension =
        orderedLabelsAndDimensions
    }

    def transformedEval(
      input: IndexedVectorCollection
    ): VectorContainer =
      VectorContainer(applyOffset(rawMatrixTransform * rawMappings.modelParameterCollectionToRawVector(input)))

    def dummyMapping(@unused input: IndexedVectorCollection): IndexedMatrixCollection =
      IndexedMatrixCollection(index = Map())

    for {
      evalCache <- CachedComputation.of[F, VectorContainer](transformedEval, evalCacheDepth)
      jacobianCache <-
        CachedComputation.of[F, IndexedMatrixCollection](dummyMapping, None)
    } yield LinearForwardModel[F](evalCache, jacobianCache, transform, vectorOffset, domainDimension, rangeDimension)
  }

  @unused
  private[thylacine] def of[F[_]: STM: Async](
    identifier: ModelParameterIdentifier,
    values: Vector[Vector[Double]],
    evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] =
    of[F](
      transform      = IndexedMatrixCollection(identifier, values),
      vectorOffset   = None,
      evalCacheDepth = evalCacheDepth
    )

  def of[F[_]: STM: Async](
    label: String,
    values: Vector[Vector[Double]],
    evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] =
    of[F](
      transform      = IndexedMatrixCollection(ModelParameterIdentifier(label), values),
      vectorOffset   = None,
      evalCacheDepth = evalCacheDepth
    )

  @unused
  def identityOf[F[_]: STM: Async](
    label: String,
    dimension: Int,
    evalCacheDepth: Option[Int]
  ): F[LinearForwardModel[F]] =
    of[F](
      transform = IndexedMatrixCollection
        .squareIdentity(ModelParameterIdentifier(label), dimension),
      vectorOffset   = None,
      evalCacheDepth = evalCacheDepth
    )
}
