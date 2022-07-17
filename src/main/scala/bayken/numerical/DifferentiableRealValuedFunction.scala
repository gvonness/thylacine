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

import scala.annotation.tailrec

trait DifferentiableRealValuedFunction[+T <: RealValuedFunction] extends RealValuedFunction {
  def derivative: T

  def solveFor(
      input: Double,
      initialGuess: Double = 0,
      tolerance: Double = 0.000001,
      maxIterations: Int = 100
  ): Double =
    evalRecursion(initialGuess, input, 0, tolerance, maxIterations)

  @tailrec
  private def evalRecursion(
      x: Double,
      target: Double,
      currentIteration: Int,
      tolerance: Double,
      maxIterations: Int
  ): Double = {
    val nextNewtonMethodStep = {
      x - (this.evalAt(
        x
      ) - target) / derivative.evalAt(x)
    }

    if (
      Math.abs(
        x - nextNewtonMethodStep
      ) <= tolerance || currentIteration >= maxIterations
    ) {
      nextNewtonMethodStep
    } else {
      evalRecursion(nextNewtonMethodStep, target, currentIteration + 1, tolerance, maxIterations)
    }
  }
}
