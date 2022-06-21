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

sealed trait Abscissa {
  def min: Double
  def max: Double
  def abscissaPoints: List[Double]
  def scale: Double = max - min
}

object Abscissa {

  case class LegendreAbscissa(
      quadrature: LegendreQuadrature
  ) extends Abscissa {

    lazy val abscissaPoints: List[Double] =
      quadrature.getPolesAndWeights(min, max).map(_._1)

    override lazy val min: Double = abscissaPoints.min

    override lazy val max: Double = abscissaPoints.max
  }

  case class UniformAbscissa(min: Double, max: Double, numPoints: Int)
      extends Abscissa {
    private val intervalLength: Double = max - min
    lazy val differential: Double      = (max - min) / (numPoints - 1)

    lazy val abscissaPoints: List[Double] = (0 until numPoints)
      .map(i => min + i.toDouble / (numPoints - 1) * intervalLength)
      .toList
  }
}
