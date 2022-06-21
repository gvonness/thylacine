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

import bayken.numerical.{
  PiecewisePolynomial1DSupport,
  RealValuedFunction,
  arclength
}

case class ArcLengthFromZero(piecewisePolynomial: PiecewisePolynomial1DSupport)
    extends RealValuedFunction {

  private val arcLengthCalculation: ArcLengthCalculation =
    arclength.ArcLengthCalculation(piecewisePolynomial)

  val arcLengthIntegrand: RealValuedFunction =
    arcLengthCalculation.arcLengthIntegrand

  override def evalAt(x: Double): Double =
    arcLengthCalculation.arcLengthBetween(0d, x)
}
