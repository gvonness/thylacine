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
package bayken.model

import bayken.numerical.LegendreQuadrature

case class ValueStatistics(
    quadrature: LegendreQuadrature,
    lowerBound: Double,
    upperBound: Double,
    values: List[Double]
) {
  assert(quadrature.order == values.size)

  private val polesAndWeights =
    quadrature.getPolesAndWeights(lowerBound, upperBound)

  lazy val definiteIntegral: Double =
    polesAndWeights.map(_._2).zip(values).map(i => i._1 * i._2).sum

  lazy val normalisedCdf: List[Double] = {
    val (total, unscaledAccumulation) =
      values
        .zip(polesAndWeights.map(_._2))
        .map(i => i._1 * i._2)
        .foldLeft((0d, List[Double]())) { (i, j) =>
          val currentSum = i._1 + j
          (currentSum, currentSum :: i._2)
        }

    unscaledAccumulation.reverse.map(_ / total)
  }

  lazy val median: Double =
    normalisedCdf
      .zip(polesAndWeights.map(_._1))
      .minBy(i => Math.abs(i._1 - .5))
      ._2

  lazy val integratedMean: Double = {
    polesAndWeights
      .map(i => i._1 * i._2)
      .zip(values)
      .map(i => i._1 * i._2)
      .sum / definiteIntegral
  }

  lazy val standardDeviation: Double =
    Math.sqrt(
      polesAndWeights
        .map(i => i._2 * Math.pow(i._1 - integratedMean, 2))
        .zip(values)
        .map(i => i._1 * i._2)
        .sum / definiteIntegral
    )

  def centralNormalisedMoment(momentOrder: Int): Double =
    polesAndWeights
      .map(i => i._2 * Math.pow(i._1 - integratedMean, momentOrder.toDouble))
      .zip(values)
      .map(i => i._1 * i._2)
      .sum / Math.pow(integratedMean, momentOrder.toDouble)

  lazy val valueMean: Double =
    values.sum / values.size

  lazy val (valueConfidenceInterval95Min, valueConfidenceInterval95Max) = {
    val result = values
      .sortBy(i => Math.abs(i - valueMean))
      .dropRight(Math.ceil(values.size * .05).toInt)
    (result.min, result.max)
  }
}
