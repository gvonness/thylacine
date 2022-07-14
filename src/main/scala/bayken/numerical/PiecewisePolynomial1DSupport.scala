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

import bayken.numerical.Interval1D.{NegativeUnbounded, OrderedBoundedInterval1D, PositiveUnbounded}
import bayken.numerical.Interval1DCollection.NontrivialInterval1DCollection
import bayken.numerical.Polynomial1D.NonTrivialPolynomial

// Piecewise polynomial support; i.e. it is only necessary to define the non-trivial
// function (support). This class is closed under differentiation (i.e. derivatives
// of linear functions will be removed from the support in the result)
case class PiecewisePolynomial1DSupport(
    polynomialMapping: PairwiseDisjointDomainMapping[NonTrivialPolynomial]
) extends DifferentiableRealValuedFunction[PiecewisePolynomial1DSupport] {

  override lazy val derivative: PiecewisePolynomial1DSupport =
    PiecewisePolynomial1DSupport(
      PairwiseDisjointDomainMapping(polynomialMapping.domainMapping.map { m =>
        m._1 -> m._2.derivative
      }.collect { case (d, f @ NonTrivialPolynomial(_)) =>
        d -> f
      })
    )

  override def evalAt(x: Double): Double =
    polynomialMapping.retrieveMappedEntity(x).map(_.evalAt(x)).getOrElse(0d)
}

object PiecewisePolynomial1DSupport {

  def fitPiecewisePolynomial(
      boundaries: Seq[Double],
      polnomialOrders: Seq[Int],
      points: Seq[Point2D]
  ): PiecewisePolynomial1DSupport = {
    assert(boundaries.size + 1 == polnomialOrders.size)
    assert(boundaries.nonEmpty)

    val lowerDomain: NontrivialInterval1DCollection =
      NontrivialInterval1DCollection(
        Set(NegativeUnbounded(OpenBoundary(boundaries.head)))
      )

    val upperDomain: NontrivialInterval1DCollection =
      NontrivialInterval1DCollection(
        Set(PositiveUnbounded(ClosedBoundary(boundaries.head)))
      )

    val unboundedPolnomialFirst: Map[NontrivialInterval1DCollection, NonTrivialPolynomial] = Map(
      lowerDomain -> Polynomial1D.fit(
        points.filter(p => lowerDomain.contains(p.x)).toList,
        polnomialOrders.head
      ),
      upperDomain -> Polynomial1D.fit(
        points.filter(p => upperDomain.contains(p.x)).toList,
        polnomialOrders.last
      )
    )

    if (boundaries.size == 1) {
      PiecewisePolynomial1DSupport(
        PairwiseDisjointDomainMapping(
          unboundedPolnomialFirst
        )
      )
    } else {
      PiecewisePolynomial1DSupport(
        PairwiseDisjointDomainMapping[NonTrivialPolynomial](
          unboundedPolnomialFirst ++ boundaries.init
            .zip(boundaries.tail)
            .zip(polnomialOrders.tail.init)
            .map { i =>
              val domain = NontrivialInterval1DCollection(
                Set(
                  OrderedBoundedInterval1D(ClosedBoundary(i._1._1), OpenBoundary(i._1._2))
                )
              )

              domain -> Polynomial1D
                .fit(points.filter(p => domain.contains(p.x)).toList, i._2)
            }
            .toMap
        )
      )
    }
  }

  def constructPiecewisePolynomial(
      boundaries: Seq[Double],
      nonTrivialPolynomials: Seq[NonTrivialPolynomial]
  ): PiecewisePolynomial1DSupport = {
    assert(boundaries.size + 1 == nonTrivialPolynomials.size)
    assert(boundaries.nonEmpty)

    val lowerDomain: NontrivialInterval1DCollection =
      NontrivialInterval1DCollection(
        Set(NegativeUnbounded(OpenBoundary(boundaries.head)))
      )

    val upperDomain: NontrivialInterval1DCollection =
      NontrivialInterval1DCollection(
        Set(PositiveUnbounded(ClosedBoundary(boundaries.head)))
      )

    val unboundedPolnomialFirst: Map[NontrivialInterval1DCollection, NonTrivialPolynomial] = Map(
      lowerDomain -> nonTrivialPolynomials.head,
      upperDomain -> nonTrivialPolynomials.last
    )

    if (boundaries.size == 1) {
      PiecewisePolynomial1DSupport(
        PairwiseDisjointDomainMapping(
          unboundedPolnomialFirst
        )
      )
    } else {
      PiecewisePolynomial1DSupport(
        PairwiseDisjointDomainMapping[NonTrivialPolynomial](
          unboundedPolnomialFirst ++ boundaries.init
            .zip(boundaries.tail)
            .zip(nonTrivialPolynomials.tail.init)
            .map { i =>
              val domain = NontrivialInterval1DCollection(
                Set(
                  OrderedBoundedInterval1D(ClosedBoundary(i._1._1), OpenBoundary(i._1._2))
                )
              )

              domain -> i._2
            }
            .toMap
        )
      )
    }
  }
}
