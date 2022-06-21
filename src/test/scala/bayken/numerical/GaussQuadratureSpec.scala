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
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class GaussQuadratureSpec extends AnyFlatSpec {

  "integrate" should "correctly calculate definite integrals of polynomials up to theoretical order limits" in {
    val quadrature = LegendreQuadrature(2)

    quadrature.integrate(Polynomial1D(List(0, 0, 0, 2)),
                         -2,
                         3
    ) shouldBe (32.5 +- .0000000001)
  }

}

object GaussQuadratureSpec {}
