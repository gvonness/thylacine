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
package thylacine.model.integration.slq

import thylacine.model.core.Erratum.{ResultOrErrIo, _}
import thylacine.model.core._

private[thylacine] case class PointInInterval(
    point: Double,
    lowerBound: Double,
    upperBound: Double,
    validated: Boolean = false
) extends CanValidate[PointInInterval] {
  if (!validated) {
    assert(upperBound > lowerBound)
    assert(point >= lowerBound)
    assert(point < upperBound)
  }

  override lazy val getValidated: PointInInterval =
    this.copy(validated = true)

  private[thylacine] lazy val intervalLength: ResultOrErrIo[Double] =
    ResultOrErrIo.fromCalculation {
      Math.abs(upperBound - lowerBound)
    }

  private[thylacine] lazy val symmetrize: ResultOrErrIo[PointInInterval] = {
    val length1 = upperBound - point
    val length2 = point - lowerBound
    ResultOrErrIo.fromCalculation {
      if (length1 > length2) {
        this.copy(upperBound = point + length2)
      } else if (length1 < length2) {
        this.copy(lowerBound = point - length1)
      } else {
        this
      }
    }
  }

  private[thylacine] def isIntersectingWith(input: PointInInterval): ResultOrErrIo[Boolean] =
    ResultOrErrIo.fromCalculation {
      (upperBound > input.lowerBound &&
        lowerBound < input.upperBound) ||
      (input.upperBound > lowerBound &&
        input.lowerBound < upperBound)
    }

  private[thylacine] def distanceSquaredFrom(input: PointInInterval): ResultOrErrIo[Double] =
    ResultOrErrIo.fromCalculation(Math.pow(point - input.point, 2))

  // Sampling from a linearly scaled version of this interval (1 corresponding to
  // the original interval and 0 to the central point)
  private[thylacine] def getSample(scaleParameter: Double): ResultOrErrIo[Double] =
    ResultOrErrIo.fromCalculation {
      (Math.random() - 0.5) * scaleParameter * (upperBound - lowerBound) + point
    }
}

private[thylacine] object PointInInterval {

  // Convenience method to generate a placeholder
  // interval (that's valid)
  private[thylacine] def apply(point: Double): PointInInterval =
    PointInInterval(
      point = point,
      lowerBound = point - 1,
      upperBound = point + 1,
      validated = true
    )

  private[thylacine] def findDisjointBoundary(
      pii1: PointInInterval,
      pii2: PointInInterval
  ): ResultOrErrIo[(PointInInterval, PointInInterval)] = {

    val isPii1Larger = pii1.point > pii2.point
    val (largerPii, smallerPii) =
      if (isPii1Larger) (pii1, pii2) else (pii2, pii1)

    val averageBoundary = (pii1.point + pii2.point) / 2.0

    val boundaryValueSpec: ResultOrErrIo[Double] =
      if (largerPii.lowerBound <= averageBoundary && smallerPii.upperBound > averageBoundary) {
        ResultOrErrIo.fromValue(averageBoundary)
      } else if (largerPii.lowerBound <= averageBoundary) {
        ResultOrErrIo.fromValue(smallerPii.upperBound)
      } else if (smallerPii.upperBound > averageBoundary) {
        ResultOrErrIo.fromValue(largerPii.lowerBound)
      } else {
        ResultOrErrIo.fromErratum(
          UnexpectedErratum(
            "Can't find boundary between cubes that are already disjoint!"
          )
        )
      }

    for {
      boundaryValue <- boundaryValueSpec
    } yield
      if (isPii1Larger) {
        (pii1.copy(lowerBound = boundaryValue), pii2.copy(upperBound = boundaryValue))
      } else {
        (pii1.copy(upperBound = boundaryValue), pii2.copy(lowerBound = boundaryValue))
      }
  }
}
