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
package thylacine.util

import thylacine.model.core.Erratum.{ResultOrErrIo, UnexpectedErratum}

private[thylacine] object MathOps {

  private[thylacine] def trapezoidalQuadrature(
      abscissa: Vector[Double],
      values: Vector[Double]
  ): ResultOrErrIo[Double] =
    if (abscissa.size == values.size && abscissa.size > 1) {
      trapezoidalQuadrature(abscissa.zip(values))
    } else {
      ResultOrErrIo.fromErratum(
        UnexpectedErratum("Malformed abscissa for trapezoidal quadrature")
      )
    }

  private[thylacine] def trapezoidalQuadrature(
      graphPoints: Vector[(Double, Double)]
  ): ResultOrErrIo[Double] =
    if (
      graphPoints.size > 1 && graphPoints
        .map(_._1)
        .toSet
        .size == graphPoints.size
    ) {
      ResultOrErrIo.fromCalculation {
        val sorted = graphPoints.sortBy(_._1)
        sorted
          .dropRight(1)
          .zip(sorted.tail)
          .map { graphPointPair =>
            0.5 * (graphPointPair._1._2 + graphPointPair._2._2) * (graphPointPair._2._1 - graphPointPair._1._1)
          }
          .sum
      }
    } else {
      ResultOrErrIo.fromErratum(
        UnexpectedErratum("Malformed abscissa for trapezoidal quadrature")
      )
    }

  // Creates a discretised CDF that facilitates sampling over a
  // discrete set of outcomes via uniform sampling on [0, 1)
  private[thylacine] def cdfStaircase(
      values: Vector[BigDecimal]
  ): ResultOrErrIo[Vector[(BigDecimal, BigDecimal)]] =
    for {
      cdfReversed <- ResultOrErrIo.fromCalculation {
                       values.foldLeft(Vector[BigDecimal](BigDecimal(0))) { (i, j) =>
                         (i.head + j) +: i
                       }
                     }
      normalisedCdf <- ResultOrErrIo.fromCalculation {
                         cdfReversed.map(_ / cdfReversed.head).reverse
                       }
      result <- ResultOrErrIo.fromCalculation {
                  normalisedCdf.dropRight(1).zip(normalisedCdf.tail)
                }
    } yield result

}
