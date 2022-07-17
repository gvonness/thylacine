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

import bayken.numerical.Interval1D.OrderedBoundedInterval1D
import bayken.numerical.Interval1DCollection.NontrivialInterval1DCollection
import bayken.numerical.Polynomial1D.NonTrivialPolynomial
import bayken.numerical.arclength.ArcLengthSpec.ArcLengthFixture
import bayken.numerical._

import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ArcLengthSpec extends AnyFlatSpec {

  "ArcLengthCalculation" should "calculate the arc-length of a function between two points" in new ArcLengthFixture {

    arcLengthCalculator.arcLengthBetween(-2, 3) shouldBe (8.610525434957797277 +- .000000000001)
  }

  "ArcLengthFromZero" should "calculate the arc-length from x=0 for a polynomial correctly" in new ArcLengthFixture {

    arcLengthFromZeroCalculator.evalAt(
      4
    ) shouldBe (9.2935675248658717467652311 +- .000000000001)
  }

  it should "calculate the arc-length from x=0 for a piecewise polynomial correctly" in new ArcLengthFixture {

    piecewiseArcLengthFromZeroCalculator.evalAt(
      2.1
    ) shouldBe (23.273656333308885 +- .000000000001)
  }

  "ArcLengthFromZeroInverse" should "calculate the x coordinate for a given arc-length and a polynomial correctly" in new ArcLengthFixture {

    inverseArcLengthFromZeroCalculator.evalAt(
      4.79905
    ) shouldBe (2.7181993142857817 +- .000000000001)
  }

  it should "calculate the x coordinate for a given arc-length and a piecewise polynomial correctly" in new ArcLengthFixture {

    piecewiseInverseArcLengthFromZeroCalculator.evalAt(
      27.73336
    ) shouldBe (2.1728693044798457 +- .000000000001)
  }
}

object ArcLengthSpec {

  trait ArcLengthFixture {

    val domain: NontrivialInterval1DCollection = NontrivialInterval1DCollection(
      Set(OrderedBoundedInterval1D.closedInterval(-5, 5))
    )

    val piecewiseDomain1: NontrivialInterval1DCollection =
      NontrivialInterval1DCollection(
        Set(OrderedBoundedInterval1D(ClosedBoundary(-1d), OpenBoundary(1.5)))
      )

    val piecewiseDomain2: NontrivialInterval1DCollection =
      NontrivialInterval1DCollection(
        Set(OrderedBoundedInterval1D.closedInterval(1.5, 5.2))
      )

    val domainMapping: PairwiseDisjointDomainMapping[NonTrivialPolynomial] =
      PairwiseDisjointDomainMapping(
        Map(
          domain -> Polynomial1D(List(0, 0, 0.5))
            .asInstanceOf[NonTrivialPolynomial]
        )
      )

    val piecewiseDomainMapping: PairwiseDisjointDomainMapping[NonTrivialPolynomial] =
      PairwiseDisjointDomainMapping(
        Map(
          piecewiseDomain1 -> Polynomial1D(List(0, 0.25))
            .asInstanceOf[NonTrivialPolynomial],
          piecewiseDomain2 -> Polynomial1D(List(0, 0, 0, 0, -1.5))
            .asInstanceOf[NonTrivialPolynomial]
        )
      )

    val polynomial: PiecewisePolynomial1DSupport = PiecewisePolynomial1DSupport(
      domainMapping
    )

    val piecewisePolynomial: PiecewisePolynomial1DSupport =
      PiecewisePolynomial1DSupport(piecewiseDomainMapping)

    val arcLengthCalculator: ArcLengthCalculation = ArcLengthCalculation(
      polynomial
    )

    val arcLengthFromZeroCalculator: ArcLengthFromZero = ArcLengthFromZero(
      polynomial
    )

    val piecewiseArcLengthFromZeroCalculator: ArcLengthFromZero =
      ArcLengthFromZero(piecewisePolynomial)

    val inverseArcLengthFromZeroCalculator: ArcLengthFromZeroInverse =
      ArcLengthFromZeroInverse(arcLengthFromZeroCalculator, .000000000001)

    val piecewiseInverseArcLengthFromZeroCalculator: ArcLengthFromZeroInverse =
      ArcLengthFromZeroInverse(piecewiseArcLengthFromZeroCalculator, .000000000001)
  }
}
