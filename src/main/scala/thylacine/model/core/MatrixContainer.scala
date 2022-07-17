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

import breeze.linalg._
import cats.implicits._

import scala.{Vector => ScalaVector}

private[thylacine] case class MatrixContainer(
    values: Map[(Int, Int), Double],
    rowTotalNumber: Int,
    columnTotalNumber: Int,
    validated: Boolean = false
) extends Container
    with CanValidate[MatrixContainer] {
  if (!validated) {
    assert(values.keys.map(_._1).max <= rowTotalNumber)
    assert(values.keys.map(_._1).min >= 1)
    assert(values.keys.map(_._2).max <= columnTotalNumber)
    assert(values.keys.map(_._2).min >= 1)
  }

  private[thylacine] override lazy val getValidated: MatrixContainer =
    if (validated) this else this.copy(validated = true)

  private[thylacine] lazy val isSquare: Boolean =
    rowTotalNumber == columnTotalNumber

  // Low-level API
  // ------------------------
  private[thylacine] lazy val rawMatrix: DenseMatrix[Double] = {
    val matResult: DenseMatrix[Double] =
      DenseMatrix.zeros[Double](rowTotalNumber, columnTotalNumber)
    values.foreach { i =>
      matResult.update(i._1._1 - 1, i._1._2 - 1, i._2)
    }
    matResult
  }

  // Extension of matrices with the input being used to increase
  // the number of columns, under the assumption that row numbers
  // are equal and have been checked outside of this
  private[thylacine] def columnMergeWith(
      input: MatrixContainer
  ): MatrixContainer =
    MatrixContainer(
      values ++ input.getValidated.values.map(i => (i._1._1, i._1._2 + columnTotalNumber) -> i._2),
      rowTotalNumber = rowTotalNumber,
      columnTotalNumber = columnTotalNumber + input.columnTotalNumber,
      validated = true
    )

  // Analogous to the above for columns
  private[thylacine] def rowMergeWith(input: MatrixContainer): MatrixContainer =
    MatrixContainer(
      values ++ input.getValidated.values.map(i => (i._1._1 + rowTotalNumber, i._1._2) -> i._2),
      rowTotalNumber = rowTotalNumber + input.rowTotalNumber,
      columnTotalNumber = columnTotalNumber,
      validated = true
    )

  // Diagonally combines two matrices with zero'd upper-right
  // and lower-left submatrices
  private[thylacine] def diagonalMergeWith(
      input: MatrixContainer
  ): MatrixContainer =
    MatrixContainer(
      values ++ input.getValidated.values.map(i => (i._1._1 + rowTotalNumber, i._1._2 + columnTotalNumber) -> i._2),
      rowTotalNumber = rowTotalNumber + input.columnTotalNumber,
      columnTotalNumber = columnTotalNumber + input.columnTotalNumber,
      validated = true
    )
}

private[thylacine] object MatrixContainer {

  private[thylacine] def zeros(
      rowDimension: Int,
      columnDimension: Int
  ): MatrixContainer =
    MatrixContainer(
      values = Map(),
      rowTotalNumber = rowDimension,
      columnTotalNumber = columnDimension,
      validated = true
    )

  private[thylacine] def apply(
      input: ScalaVector[ScalaVector[Double]]
  ): MatrixContainer = {
    val valueMap = input
      .foldLeft((1, Map[(Int, Int), Double]())) { (i, j) =>
        (i._1 + 1,
         j.foldLeft((1, i._2)) { (k, l) =>
           (k._1 + 1, k._2 + ((i._1, k._1) -> l))
         }._2
        )
      }
      ._2
    MatrixContainer(
      values = valueMap,
      rowTotalNumber = valueMap.keySet.map(_._1).max,
      columnTotalNumber = valueMap.keySet.map(_._2).max
    )
  }
}
