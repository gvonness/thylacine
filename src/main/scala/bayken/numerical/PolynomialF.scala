package ai.entrolution
package bayken.numerical

import bayken.model.Point2D

import breeze.linalg.{DenseMatrix, DenseVector}

sealed trait PolynomialF {
  val coefficients: List[Double]
  val derivative: PolynomialF
  final lazy val order = coefficients.size - 1
  def evalAt(x: Double): Double
}

object PolynomialF {

  def apply(coefficients: List[Double]): PolynomialF =
    if (coefficients.exists(i => i != 0)) {
      NonTrivialPolynomial(
        coefficients.reverse.dropWhile(i => i == 0).reverse
      )
    } else {
      TrivialPolynomial
    }

  def fit(points: List[Point2D], order: Int): PolynomialF = {
    // Test to ensure all x values are distinct. This ensures the
    // Vandermonde matrix is invertible
    assert(points.map(_.x).toSet.size == points.size)

    val xMat = DenseMatrix.zeros[Double](points.size, order + 1)

    points.foldLeft(0) { (i, j) =>
      xMat(i, ::) := DenseVector(
        (0 to order).map(k => Math.pow(j.x, k.toDouble)).toArray
      ).t
      i + 1
    }

    val y = DenseVector(points.map(_.y).toArray)

    PolynomialF(((xMat.t * xMat) \ (xMat.t * y)).toArray.toList)
  }

  case class NonTrivialPolynomial(coefficients: List[Double])
      extends PolynomialF {
    assert(coefficients.exists(i => i != 0))

    override lazy val derivative: PolynomialF =
      coefficients match {
        case _ :: t =>
          PolynomialF(
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

  case object TrivialPolynomial extends PolynomialF {
    override val coefficients: List[Double] = List(0)
    override val derivative: PolynomialF    = TrivialPolynomial

    override def evalAt(x: Double): Double = 0
  }
}
