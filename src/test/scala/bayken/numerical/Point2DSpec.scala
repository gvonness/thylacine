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

class Point2DSpec extends AnyFlatSpec {
  "Point2D.rotateAboutOrigin" should "correctly rotate counter clockwise points about the origin" in {
    val oneOverSqrtTwo = 1.0 / Math.sqrt(2.0)

    val rotatedPoint =
      Point2D.rotateAboutOrigin(Point2D(oneOverSqrtTwo, oneOverSqrtTwo),
                                Math.PI / 4.0
      )

    rotatedPoint.x mustBe (0d +- .000001)
    rotatedPoint.y mustBe (1d +- .000001)
  }

  it should "correctly rotate clockwise points about the origin" in {
    val rotatedPoint =
      Point2D.rotateAboutOrigin(Point2D(0d, 1d), -Math.PI / 2.0)

    rotatedPoint.x mustBe (1d +- .000001)
    rotatedPoint.y mustBe (0d +- .000001)
  }
}
