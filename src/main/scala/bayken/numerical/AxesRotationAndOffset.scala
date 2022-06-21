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

import breeze.linalg._

import scala.annotation.tailrec

// This contains the computation needed to determine the rotation and offset
// of our coordinate system to capture the rotation of our blade model due to the
// holder placement
case class AxesRotationAndOffset[+T <: RealValuedFunction](
    fn: DifferentiableRealValuedFunction[T],
    tolerance: Double
) {

  private def getNextIteration(
      p1: Point2D,
      p2: Point2D,
      guess: DenseVector[Double]
  ): DenseVector[Double] =
    guess.toScalaVector.toList match {
      case theta :: y :: l1 :: l2 :: _ =>
        val cosT = Math.cos(theta)
        val sinT = Math.sin(theta)
        val fl1  = fn.evalAt(l1)
        val fl2  = fn.evalAt(l2)
        val dfl1 = fn.derivative.evalAt(l1)
        val dfl2 = fn.derivative.evalAt(l2)

        val Jacobian = DenseMatrix.zeros[Double](4, 4)

        Jacobian(0, ::) := DenseVector(
          List[Double](-l1 * sinT - fl1 * cosT,
                       0,
                       cosT - dfl1 * sinT,
                       0
          ).toArray
        ).t
        Jacobian(1, ::) := DenseVector(
          List[Double](l1 * cosT - fl1 * sinT, 1, sinT + dfl1 * sinT, 0).toArray
        ).t
        Jacobian(2, ::) := DenseVector(
          List[Double](-l2 * sinT - fl2 * cosT,
                       0,
                       0,
                       cosT - dfl2 * sinT
          ).toArray
        ).t
        Jacobian(3, ::) := DenseVector(
          List[Double](l2 * cosT - fl2 * sinT, 1, 0, sinT + dfl2 * cosT).toArray
        ).t

        val inverseJacobian = inv(Jacobian)

        DenseVector(
          List(theta, y, l1, l2).toArray
        ) - inverseJacobian * DenseVector(
          List(l1 * cosT - fl1 * sinT - p1.x,
               l1 * sinT + fl1 * cosT + y - p1.y,
               l2 * cosT - fl2 * sinT - p2.x,
               l2 * sinT + fl2 * cosT + y - p2.y
          ).toArray
        )
      case _ =>
        DenseVector.zeros[Double](4)
    }

  @tailrec
  private def evalRecursion(
      p1: Point2D,
      p2: Point2D,
      x: DenseVector[Double],
      currentIteration: Int
  ): DenseVector[Double] = {
    val nextNewtonMethodStep: DenseVector[Double] = getNextIteration(p1, p2, x)

    if (norm(x - nextNewtonMethodStep) <= tolerance) {
      nextNewtonMethodStep
    } else {
      evalRecursion(p1, p2, nextNewtonMethodStep, currentIteration + 1)
    }
  }

  def getRotationCorrectionFor(p1: Point2D, p2: Point2D): Double = {
    val initialGuess = DenseVector(
      List(0, (p1.y + p2.y) / 2.0, p1.x, p2.x).toArray
    )

    evalRecursion(p1, p2, initialGuess, 0).toScalaVector.toList.head
  }
}
