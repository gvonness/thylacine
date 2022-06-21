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

import breeze.linalg.{DenseMatrix, DenseVector, eig}
import breeze.linalg.eig.Eig

sealed trait GaussQuadratureBuilder {
  def order: Int

  def getPolesAndWeights(
      lowerBound: Double,
      upperBound: Double
  ): List[(Double, Double)]

  def integrate(
      fn: RealValuedFunction,
      lowerBound: Double,
      upperBound: Double
  ): Double
}

case class LegendreQuadrature(override val order: Int)
    extends GaussQuadratureBuilder {
  private val a = List.fill(order)(0d)

  private val b =
    (1 until order)
      .map(i => i / Math.sqrt(4.0 * Math.pow(i.toDouble, 2d) - 1d))
      .toList

  private val mu0 = 2d

  private lazy val jacobi: List[(Double, Double)] =
    GaussQuadratureBuilder.buildJacobi(a, b, mu0)

  override def getPolesAndWeights(
      lowerBound: Double,
      upperBound: Double
  ): List[(Double, Double)] =
    GaussQuadratureBuilder.rescalePolesAndWeights(
      jacobi,
      lowerBound,
      upperBound
    )

  override def integrate(
      fn: RealValuedFunction,
      lowerBound: Double,
      upperBound: Double
  ): Double =
    getPolesAndWeights(lowerBound, upperBound)
      .map(pw => fn.evalAt(pw._1) * pw._2)
      .sum
}

object GaussQuadratureBuilder {

  def buildJacobi(
      a: List[Double],
      b: List[Double],
      mu0: Double
  ): List[(Double, Double)] = {
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

    resultUnordered._1.zip(resultUnordered._2).sortBy(_._1)
  }

  def rescalePolesAndWeights(
      input: List[(Double, Double)],
      min: Double,
      max: Double
  ): List[(Double, Double)] = {
    val factor1 = (max - min) / 2.0
    val factor2 = (max + min) / 2.0

    input.map { i =>
      (factor1 * i._1 + factor2, factor1 * i._2)
    }
  }
}
