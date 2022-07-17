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
import bayken.numerical._

case class ArcLengthCalculation(
    piecewisePolynomial: PiecewisePolynomial1DSupport
) {

  // Arc-length calculations will not involve integrals over polynomials, as
  // square roots come into the mix. However, we know the associated integrands
  // will be smooth and will be well accurately integrated by a Legendre quadrature
  // of a suitable order. Here we create a Legendre quadrature that should be fast
  // to calculate with while yielding acceptable accuracies
  private val quadrature: LegendreQuadrature =
    LegendreQuadrature(
      Math.max(piecewisePolynomial.polynomialMapping.domainMapping.values
                 .map(_.order)
                 .max,
               50
      )
    )

  val arcLengthIntegrand: RealValuedFunction =
    AnalyticRealValuedFunction(i => Math.sqrt(1d + Math.pow(i, 2)))
      .composeWith(piecewisePolynomial.derivative)

  def arcLengthBetween(x0: Double, x1: Double): Double =
    quadrature.integrate(arcLengthIntegrand, Math.min(x0, x1), Math.max(x0, x1))

}
