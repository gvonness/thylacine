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
package bayken.util

object MathOps {

  def trapezoidalRule(values: List[Double], dx: Double): Double =
    (values.map(i => 2 * i).sum - values.head - values.last) * dx / 2.0

  def mean(values: List[Double]): Double =
    values.sum / values.size

  def confidenceInterval95(values: List[Double]): (Double, Double) = {
    val average = mean(values)
    val result = values
      .sortBy(i => Math.abs(i - average))
      .dropRight(Math.ceil(values.size * .05).toInt)
    (result.min, result.max)
  }

  // Creates a discretised CDF that facilitates sampling over a
  // discrete set of outcomes via uniform sampling on [0, 1)
  def cdfStaircase(values: List[BigDecimal]): List[(Double, Double)] = {
    val cdfReversed =
      values.foldLeft(List[BigDecimal](BigDecimal(0))) { (i, j) =>
        if (j <= 0) {
          println("Received zero cube volume!!!!")
        }

        (i.head + j) :: i
      }
    val normalisedCdf: List[BigDecimal] = cdfReversed
      .map(_ / cdfReversed.head)
      .reverse

    normalisedCdf
      .dropRight(1)
      .zip(normalisedCdf.tail)
      .map(i => (i._1.doubleValue, i._2.doubleValue))
  }

}
