/*
 * Copyright 2020-2021 Entrolution
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
package thylacine.visualisation

case class GraphPoint(x: Double, y: Double) {

  def distSquaredTo(point: GraphPoint): Double =
    Math.pow(x - point.x, 2) + Math.pow(y - point.y, 2)

  def scaledDistSquaredTo(
      point: GraphPoint,
      xScale: Double,
      yScale: Double
  ): Double =
    Math.pow((x - point.x) / xScale, 2) + Math.pow((y - point.y) / yScale, 2)

  def distanceTo(point: GraphPoint): Double =
    Math.sqrt(distSquaredTo(point))

  def add(point: GraphPoint): GraphPoint =
    GraphPoint(x + point.x, y + point.y)

  def minus(point: GraphPoint): GraphPoint =
    GraphPoint(x - point.x, y - point.y)

  def scalarMultiply(multiplier: Double): GraphPoint =
    GraphPoint(multiplier * x, multiplier * y)
}

object GraphPoint {

  def apply(input: (Double, Double)): GraphPoint =
    GraphPoint(input._1, input._2)
}
