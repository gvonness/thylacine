/*
 * Copyright 2023 Greg von Nessi
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

import scala.annotation.tailrec

private[thylacine] case class Simplex(start: GraphPoint, end: GraphPoint) {
  private lazy val differential = end.minus(start)

  private[thylacine] lazy val length: Double =
    start.distanceTo(end)

  private[thylacine] def getInterpolation(t: Double): GraphPoint =
    if (t <= 0) {
      start
    } else if (t >= 1) {
      end
    } else {
      start.add(differential.scalarMultiply(t))
    }

  @tailrec
  final private[thylacine] def getInterpolationPoints(
    start: Double,
    ds: Double,
    prev: List[GraphPoint] = List()
  ): (List[GraphPoint], Double) =
    if (start >= length) {
      (prev, start - length)
    } else {
      getInterpolationPoints(start + ds, ds, List(getInterpolation(start / length)) ++ prev)
    }
}

private[thylacine] object Simplex {

  private[thylacine] def apply(input: (GraphPoint, GraphPoint)): Simplex =
    Simplex(input._1, input._2)
}
