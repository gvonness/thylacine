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

import org.scalatest.flatspec.AnyFlatSpec

class Polynomial1DSpec extends AnyFlatSpec {
  "apply" should "correctly construct a polynomial" in {
    val polynomial = Polynomial1D(List(1, 2, 0, 2))

    assertResult(61) {
      polynomial.evalAt(3)
    }
  }

  "derivative" should "yield the correct polynomial" in {
    val polynomial = Polynomial1D(List(1, 2, 0, 2))

    assertResult(Polynomial1D(List(2, 0, 6))) {
      polynomial.derivative
    }
  }

  "fit" should "correctly fit polynomials of specified order" in {
    assertResult(Polynomial1D(List(0, 0, 0, -2))) {
      Polynomial1D.fit(List(Point2D(0, 0), Point2D(2, -16), Point2D(3, -54)), 3)
    }

    assertResult(Polynomial1D(List(1, 0, 3))) {
      Polynomial1D.fit(List(Point2D(-2, 13), Point2D(-1, 4), Point2D(4, 49)), 2)
    }
  }
}
