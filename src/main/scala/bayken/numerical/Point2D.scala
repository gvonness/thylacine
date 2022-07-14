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

case class Point2D(x: Double, y: Double) {
  lazy val magnitude: Double = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))
}

object Point2D {

  // Convention: Positive theta (radians) is counter-clock
  // Note: Very simple calculation; Breeze will incur more overhead
  // than it's worth here
  def rotateAboutOrigin(input: Point2D, theta: Double): Point2D = {
    val cosTheta = Math.cos(theta)
    val sinTheta = Math.sin(theta)

    Point2D(
      x = input.x * cosTheta - input.y * sinTheta,
      y = input.x * sinTheta + input.y * cosTheta
    )
  }

  def distanceBetween(pt1: Point2D, pt2: Point2D): Double =
    Math.sqrt(Math.pow(pt1.x - pt2.x, 2) + Math.pow(pt1.y - pt2.y, 2))

}
