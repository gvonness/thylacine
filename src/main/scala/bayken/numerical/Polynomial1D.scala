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
package bayken.numerical

import breeze.linalg.{DenseMatrix, DenseVector}

sealed trait Polynomial1D extends DifferentiableRealValuedFunction[Polynomial1D] {
  val coefficients: List[Double]
  final lazy val order = coefficients.size - 1
}

object Polynomial1D {

  def apply(coefficients: List[Double]): Polynomial1D =
    if (coefficients.exists(i => i != 0)) {
      NonTrivialPolynomial(
        coefficients.reverse.dropWhile(i => i == 0).reverse
      )
    } else {
      TrivialPolynomial
    }

  def fit(points: List[Point2D], order: Int): NonTrivialPolynomial = {
    // Test to ensure all x values are distinct. This ensures the
    // Vandermonde matrix is invertible
    assert(points.map(_.x).toSet.size == points.size)
    assert(points.size > order)

    val xMat = DenseMatrix.zeros[Double](points.size, order + 1)

    points.foldLeft(0) { (i, j) =>
      xMat(i, ::) := DenseVector(
        (0 to order).map(k => Math.pow(j.x, k.toDouble)).toArray
      ).t
      i + 1
    }

    val y = DenseVector(points.map(_.y).toArray)

    Polynomial1D(((xMat.t * xMat) \ (xMat.t * y)).toArray.toList)
      .asInstanceOf[NonTrivialPolynomial]
  }

  // Fit minimum order that will touch all points. There are more efficient
  // ways to do this; but this isn't imagined to have a big impact
  // on computation, given the generally low-order nature of the polynomials
  // used in the inference.
  def fit(points: List[Point2D]): NonTrivialPolynomial =
    fit(points, points.size - 1)

  // Extend the Vandermonde matrix to include rows specifying derivatives
  // for the polynomial
  def fit(points: List[Point2D], derivatives: List[Point2D], order: Int): NonTrivialPolynomial = {
    assert(points.map(_.x).toSet.size == points.size)
    assert(derivatives.map(_.x).toSet.size == derivatives.size)

    val xMat = DenseMatrix.zeros[Double](points.size + derivatives.size, order + 1)

    points.foldLeft(0) { (i, j) =>
      xMat(i, ::) := DenseVector(
        (0 to order).map(k => Math.pow(j.x, k.toDouble)).toArray
      ).t
      i + 1
    }

    derivatives.foldLeft(points.size) { (i, j) =>
      xMat(i, ::) := DenseVector(
        (0d +: (1 to order).map(k => k * Math.pow(j.x, (k - 1).toDouble))).toArray
      ).t
      i + 1
    }

    val y = DenseVector((points.map(_.y) ++ derivatives.map(_.y)).toArray)

    Polynomial1D(((xMat.t * xMat) \ (xMat.t * y)).toArray.toList)
      .asInstanceOf[NonTrivialPolynomial]
  }

  // Fit to get a polynomial guaranteed to touch all points and
  // align to all derivatives
  def fit(points: List[Point2D], derivatives: List[Point2D]): NonTrivialPolynomial =
    fit(points, derivatives, points.size + derivatives.size - 1)

  case class NonTrivialPolynomial(coefficients: List[Double]) extends Polynomial1D {
    assert(coefficients.exists(i => i != 0))

    override lazy val derivative: Polynomial1D =
      coefficients match {
        case _ :: t =>
          Polynomial1D(
            (1 to order).zip(t).map(i => i._1 * i._2).toList
          )
        case _ => TrivialPolynomial
      }

    override def evalAt(x: Double): Double =
      (0 to (order + 1))
        .zip(coefficients)
        .map(i => i._2 * Math.pow(x, i._1.toDouble))
        .sum
  }

  case object TrivialPolynomial extends Polynomial1D {
    override val coefficients: List[Double] = List(0)
    override val derivative: Polynomial1D   = TrivialPolynomial

    override def evalAt(x: Double): Double = 0
  }
}
