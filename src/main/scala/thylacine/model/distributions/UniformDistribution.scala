/*
 * Copyright 2023 Greg von Nessi
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
package thylacine.model.distributions

import thylacine.model.core.CanValidate
import thylacine.model.core.values.VectorContainer

import scala.collection.immutable.Vector as ScalaVector
import scala.collection.parallel.CollectionConverters.*

private[thylacine] case class UniformDistribution(
  upperBounds: VectorContainer,
  lowerBounds: VectorContainer,
  validated: Boolean = false
) extends Distribution
    with CanValidate[UniformDistribution] {

  private lazy val zippedBounds: ScalaVector[(Double, Double)] =
    lowerBounds.scalaVector.zip(upperBounds.scalaVector)

  if (!validated) {
    assert(upperBounds.dimension == lowerBounds.dimension)
    assert(!zippedBounds.exists(i => i._2 <= i._1))
  }

  override private[thylacine] lazy val getValidated: UniformDistribution =
    if (validated) {
      this
    } else {
      UniformDistribution(upperBounds.getValidated, lowerBounds.getValidated, validated = true)
    }

  override val domainDimension: Int = upperBounds.dimension

  private[thylacine] lazy val negLogVolume: Double =
    -zippedBounds.map { case (lowerBound, upperBound) =>
      Math.log(upperBound - lowerBound)
    }.sum

  private lazy val negInfinity: Double =
    Double.NegativeInfinity

  private[thylacine] lazy val zeroVector: VectorContainer =
    VectorContainer.zeros(domainDimension)

  private[thylacine] def insideBounds(input: VectorContainer): Boolean =
    zippedBounds.zip(input.scalaVector).forall {
      case ((lowerBound, upperBound), value) if value >= lowerBound && value < upperBound =>
        true
      case _ =>
        false
    }

  override private[thylacine] def logPdfAt(
    input: VectorContainer
  ): Double =
    if (insideBounds(input)) {
      negLogVolume
    } else {
      negInfinity
    }

  override private[thylacine] def logPdfGradientAt(
    input: VectorContainer
  ): VectorContainer =
    zeroVector

  private lazy val samplingScalingAndShift: ScalaVector[(Double, Double)] =
    zippedBounds.map { case (lowerBound, upperBound) =>
      (upperBound - lowerBound, lowerBound)
    }

  private[thylacine] def getRawSample: VectorContainer =
    VectorContainer(
      samplingScalingAndShift.par.map { case (scale, offset) =>
        Math.random() * scale + offset
      }.toVector
    )

}
