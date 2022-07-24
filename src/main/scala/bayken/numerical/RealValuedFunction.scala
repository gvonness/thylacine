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

trait RealValuedFunction { self =>
  def evalAt(x: Double): Double

  final def composeWith(f: RealValuedFunction): RealValuedFunction =
    (x: Double) => self.evalAt(f.evalAt(x))
}

object RealValuedFunction {

  def linearInterpolation(graphPoints: List[Point2D]): RealValuedFunction = {
    assert(graphPoints.map(_.x).toSet.size == graphPoints.size)
    val orderedPoints = graphPoints.sortBy(_.x)
    val pointPairs    = orderedPoints.init.zip(orderedPoints.tail)

    (x: Double) =>
      pointPairs
        .find(i => x >= i._1.x && x <= i._2.x)
        .map { i =>
          if (i._1.x < x && i._2.x > x) {
            (i._2.x - x) / (i._2.x - i._1.x) * (i._1.y - i._2.y) + i._2.y
          } else if (i._1.x == x) {
            i._1.y
          } else {
            i._2.y
          }
        }
        .getOrElse(0d)
  }
}
