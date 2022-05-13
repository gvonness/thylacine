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

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._

import breeze.linalg.{DenseMatrix, DenseVector}

// A linear forward model may work across more than
// one model parameter generator
case class LinearForwardModel(
    transform: IndexedMatrixCollection,
    vectorOffset: Option[VectorContainer],
    override val validated: Boolean = false
) extends ForwardModel {
  if (!validated) {
    assert(transform.index.map(_._2.rowTotalNumber).toSet.size == 1)
    assert(
      vectorOffset.forall(vo =>
        vo.dimension == transform.index.head._2.rowTotalNumber
      )
    )
  }

  private[thylacine] override lazy val getValidated: LinearForwardModel =
    if (validated) {
      this
    } else {
      LinearForwardModel(transform.getValidated,
                         vectorOffset.map(_.getValidated),
                         validated = true
      )
    }

  override protected val orderedParameterIdentifiersWithDimension
      : ResultOrErrIo[Vector[(ModelParameterIdentifier, Int)]] =
    ResultOrErrIo.fromCalculation(
      transform.index.map(i => (i._1, i._2.columnTotalNumber)).toVector
    )

  private[thylacine] override val rangeDimension: Int =
    transform.index.head._2.rowTotalNumber

  private[thylacine] override val domainDimension: Int =
    transform.index.map(_._2.columnTotalNumber).sum

  val rawMatrixTransform: ResultOrErrIo[DenseMatrix[Double]] =
    for {
      identifiersAndDimensions <- orderedParameterIdentifiersWithDimension
      result <- identifiersAndDimensions
                  .map(_._1)
                  .foldLeft(
                    ResultOrErrIo.fromValue(
                      MatrixContainer.zeros(rowDimension = rangeDimension,
                                            columnDimension = domainDimension
                      )
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

  private[thylacine] override def evalAt(
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

  private[thylacine] override def jacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection] =
    getJacobian
}

object LinearForwardModel {

  def apply(
      identifier: ModelParameterIdentifier,
      values: Vector[Vector[Double]]
  ): LinearForwardModel =
    LinearForwardModel(
      transform = IndexedMatrixCollection(identifier, values),
      vectorOffset = None
    )
}
