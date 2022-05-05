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

import cats.implicits._

// A linear forward model may work across more than
// one model parameter generator
case class LinearForwardModel(
    transform: IndexedMatrixCollection,
    vectorOffset: Option[VectorContainer],
    validated: Boolean = false
) extends ForwardModel
    with CanValidate[LinearForwardModel] {
  if (!validated) {
    assert(transform.index.map(_._2.columnTotalNumber).toSet.size == 1)
    assert(
      vectorOffset.forall(vo =>
        vo.dimension == transform.index.head._2.columnTotalNumber
      )
    )
  }

  override lazy val getValidated: LinearForwardModel =
    if (validated) {
      this
    } else {
      LinearForwardModel(transform.getValidated,
                         vectorOffset.map(_.getValidated),
                         validated = true
      )
    }

  override val rangeDimension: Int  = transform.index.head._2.rowTotalNumber
  override val domainDimension: Int = transform.index.head._2.columnTotalNumber

  private def evalMatrixProduct(
      identifier: ModelParameterIdentifier,
      inputVector: VectorContainer
  ): ResultOrErrIo[VectorContainer] =
    for {
      transformContainer <- transform.retrieveIndex(identifier)
    } yield VectorContainer(
      transformContainer.rawMatrix * inputVector.rawVector
    )

  private def evalSumOfMatrixProducts(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer] =
    for {
      products <-
        input.index.toList.traverse { case (identifer, vectorContainer) =>
          evalMatrixProduct(identifer, vectorContainer)
        }
      result <- ResultOrErrIo.fromCalculation(products.reduce(_ rawSumWith _))
    } yield result

  override def evalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer] =
    for {
      sumOfProductsContainer <- evalSumOfMatrixProducts(input)
    } yield vectorOffset.map { vo =>
      VectorContainer(sumOfProductsContainer.rawVector + vo.rawVector)
    }.getOrElse(sumOfProductsContainer)

  // Convenience method, as the Jacobian for linear models is obviously
  // constant
  val getJacobian: ResultOrErrIo[IndexedMatrixCollection] =
    ResultOrErrIo.fromValue(transform)

  override def jacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection] =
    getJacobian
}

object LinearForwardModel {

  def apply(
      identifier: ModelParameterIdentifier,
      values: List[List[Double]]
  ): LinearForwardModel =
    LinearForwardModel(
      transform = IndexedMatrixCollection(identifier, values),
      vectorOffset = None
    )
}
