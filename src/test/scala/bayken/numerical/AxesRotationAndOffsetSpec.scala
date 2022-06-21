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

import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class AxesRotationAndOffsetSpec extends AnyFlatSpec {
  "AxesRotationAndOffset" should "calculate rotational corrections correctly" in {
    val polnomial: DifferentiableRealValuedFunction[Polynomial1D] =
      Polynomial1D(List(0, 0, 0, 0, -0.1))
    val p1: Point2D = Point2D(.25, 5.0)
    val p2: Point2D = Point2D(.75, 5.1)

    val axesRotationAndOffsetCalculation =
      AxesRotationAndOffset(polnomial, .001)

    axesRotationAndOffsetCalculation
      .getRotationCorrectionFor(p1, p2)
      .get mustBe (0.2646670291363753 +- 0.0000001)
  }
}
