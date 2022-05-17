package ai.entrolution
package bayken.numerical

import breeze.linalg.{DenseMatrix, DenseVector, eig}
import breeze.linalg.eig.Eig

sealed trait GaussQuadratureBuilder {
  def order: Int

  def getPolesAndWeights(
      lowerBound: Double,
      upperBound: Double
  ): (List[Double], List[Double])
}

case class LegendreQuadratureBuilder(order: Int)
    extends GaussQuadratureBuilder {
  private val a = List.fill(order)(0d)

  private val b =
    (1 until order).map(i => i / Math.sqrt(4.0 * Math.pow(i, 2d) - 1d)).toList

  private val mu0 = 2d

  private lazy val jacobi: (List[Double], List[Double]) =
    GaussQuadratureBuilder.buildJacobi(a, b, mu0)

  def getPolesAndWeights(
      lowerBound: Double,
      upperBound: Double
  ): (List[Double], List[Double]) =
    GaussQuadratureBuilder.rescalePolesAndWeights(
      jacobi,
      lowerBound,
      upperBound
    )
}

object GaussQuadratureBuilder {

  def buildJacobi(
      a: List[Double],
      b: List[Double],
      mu0: Double
  ): (List[Double], List[Double]) = {
    assert(a.size - 1 == b.size)

    val n = a.size

    val T = DenseMatrix.zeros[Double](n, n)

    (List(None) ++ b.map(Some(_)))
      .zip(a)
      .map(i => List(i._1).flatten ++ List(i._2))
      .zip(b.map(Some(_)) ++ List(None))
      .map(i => i._1 ++ List(i._2).flatten)
      .foldLeft(0) { (i, j) =>
        if (i < 2) {
          T(i, ::) := DenseVector((j ++ List.fill(n - j.size)(0d)).toArray).t
        } else if (n - j.size - i + 1 > 0) {
          T(i, ::) := DenseVector(
            (List.fill(i - 1)(0d) ++ j ++ List
              .fill(n - j.size - i + 1)(0d)).toArray
          ).t
        } else {
          T(i, ::) := DenseVector((List.fill(n - j.size)(0d) ++ j).toArray).t
        }

        i + 1
      }

    val Eig(eigenValues, _, eigVectors) = eig(T)

    val resultUnordered =
      (eigenValues.toArray.toList,
       eigVectors(0, ::).t.toArray.toList.map(i => mu0 * Math.pow(i, 2))
      )

    val resultOrdered = resultUnordered._1.zip(resultUnordered._2).sortBy(_._1)

    (resultOrdered.map(_._1), resultOrdered.map(_._2))
  }

  def rescalePolesAndWeights(
      input: (List[Double], List[Double]),
      min: Double,
      max: Double
  ): (List[Double], List[Double]) = {
    val factor1 = (max - min) / 2.0
    val factor2 = (max + min) / 2.0

    (input._1.map(i => factor1 * i + factor2), input._2.map(i => factor1 * i))
  }
}
