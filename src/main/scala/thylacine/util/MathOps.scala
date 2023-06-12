/*
 * Copyright 2020-2023 Greg von Nessi
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
package thylacine.util

private[thylacine] object MathOps {

  private[thylacine] def trapezoidalQuadrature(
      abscissa: Vector[Double],
      values: Vector[Double]
  ): Double =
    if (abscissa.size == values.size && abscissa.size > 1) {
      trapezoidalQuadrature(abscissa.zip(values))
    } else {
      throw new RuntimeException("Malformed abscissa for trapezoidal quadrature")
    }

  private[thylacine] def trapezoidalQuadrature(
      graphPoints: Vector[(Double, Double)]
  ): Double =
    if (
      graphPoints.size > 1 && graphPoints
        .map(_._1)
        .toSet
        .size == graphPoints.size
    ) {
      val sorted = graphPoints.sortBy(_._1)
      sorted
        .dropRight(1)
        .zip(sorted.tail)
        .map { graphPointPair =>
          0.5 * (graphPointPair._1._2 + graphPointPair._2._2) * (graphPointPair._2._1 - graphPointPair._1._1)
        }
        .sum
    } else {
      throw new RuntimeException("Malformed abscissa for trapezoidal quadrature")
    }

  // Creates a discretised CDF that facilitates sampling over a
  // discrete set of outcomes via uniform sampling on [0, 1)
  private[thylacine] def cdfStaircase(
      values: Vector[BigDecimal]
  ): Vector[(BigDecimal, BigDecimal)] = {
    val cdfReversed = values
      .foldLeft(Vector[BigDecimal](BigDecimal(0))) { (i, j) =>
        (i.head + j) +: i
      }

    val normalisedCdf = cdfReversed
      .map(_ / cdfReversed.head)
      .reverse

    normalisedCdf
      .dropRight(1)
      .zip(normalisedCdf.tail)
  }

  private[thylacine] def vectorCdfStaircase(
      values: Vector[Double]
  ): Vector[(Double, Double)] = {
    val cdfReversed = values
      .foldLeft(Vector(0d)) { (i, j) =>
        (i.head + j) +: i
      }

    val normalisedCdf = cdfReversed
      .map(_ / cdfReversed.head)
      .reverse

    normalisedCdf
      .dropRight(1)
      .zip(normalisedCdf.tail)
  }

  private[thylacine] def modifyVectorIndex(input: Vector[Double])(index: Int, f: Double => Double): Vector[Double] =
    input.updated(index, f(input(index)))

  private[thylacine] def scalarMultipleVector(input: Vector[Double], multiplier: Double): Vector[Double] =
    input.map(multiplier * _)

  private[thylacine] def vectorMagnitude(input: Vector[Double]): Double =
    Math.sqrt(vectorMagnitudeSquared(input))

  private[thylacine] def vectorMagnitudeSquared(input: Vector[Double]): Double =
    input.map(Math.pow(_, 2.0)).sum

  private[thylacine] def vectorDotProduct(input1: Vector[Double], input2: Vector[Double]): Double =
    input1
      .zip(input2)
      .map { case (coord1, coord2) =>
        coord1 * coord2
      }
      .sum

  private[thylacine] def vectorAddition(input1: Vector[Double], input2: Vector[Double]): Vector[Double] =
    input1.zip(input2).map { case (coord1, coord2) =>
      coord1 + coord2
    }

}
