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
package bayken.numerical.arclength

import bayken.numerical.Interval1D.{NegativeUnbounded, OrderedBoundedInterval1D, PositiveUnbounded}
import bayken.numerical.Interval1DCollection.NontrivialInterval1DCollection
import bayken.numerical.RealValuedFunction

import scala.annotation.tailrec

case class ArcLengthFromZeroInverse(
    arcLengthFromZero: ArcLengthFromZero,
    tolerance: Double,
    maxIterations: Int = 100
) extends RealValuedFunction {

  private val nonTrivialIntervals: NontrivialInterval1DCollection =
    arcLengthFromZero.piecewisePolynomial.polynomialMapping.domainMapping.keySet
      .reduce(_ union _)

  private def getClosesPointToDomain(x: Double) =
    if (nonTrivialIntervals.contains(x)) {
      x
    } else {
      val intervalMidpoints: Set[Double] =
        nonTrivialIntervals.sets.collect { case OrderedBoundedInterval1D(lb, ub) =>
          (lb.boundaryValue + ub.boundaryValue) / 2.0
        }

      lazy val unboundedPoints: Set[Double] = nonTrivialIntervals.sets.collect {
        case NegativeUnbounded(bv) =>
          bv.boundaryValue - 1.0
        case PositiveUnbounded(bv) =>
          bv.boundaryValue + 1.0
      }

      (if (intervalMidpoints.nonEmpty) {
         intervalMidpoints
       } else if (unboundedPoints.nonEmpty) {
         unboundedPoints
       } else {
         // Only have sets of zero measure, which should not happen.
         // However, there is nothing strictly prohibiting this
         Set(0d)
       }).minBy(bv => Math.abs(bv - x))
    }

  private def getNewtonIteration(x: Double, target: Double): Double =
    x - (arcLengthFromZero.evalAt(
      x
    ) - target) / arcLengthFromZero.arcLengthIntegrand
      .evalAt(x)

  @tailrec
  private def evalRecursion(
      x: Double,
      target: Double,
      currentIteration: Int
  ): Double = {
    val nextNewtonMethodStep = getNewtonIteration(x, target)

    if (
      Math.abs(
        x - nextNewtonMethodStep
      ) <= tolerance || currentIteration >= maxIterations
    ) {
      nextNewtonMethodStep
    } else {
      evalRecursion(nextNewtonMethodStep, target, currentIteration + 1)
    }
  }

  override def evalAt(x: Double): Double =
    evalRecursion(getClosesPointToDomain(x), x, 0)

}
