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
package thylacine.model.core

import thylacine.model.core.Erratum.{ResultOrErrIo, _}

import scala.collection.parallel.CollectionConverters._
import scala.{Vector => ScalaVector}

private[thylacine] case class UniformBeliefModel(
    upperBounds: VectorContainer,
    lowerBounds: VectorContainer,
    validated: Boolean = false
) extends BeliefModel
    with CanValidate[UniformBeliefModel] {

  private lazy val zippedBounds: ScalaVector[(Double, Double)] =
    lowerBounds.scalaVector.zip(upperBounds.scalaVector)

  if (!validated) {
    assert(upperBounds.dimension == lowerBounds.dimension)
    assert(!zippedBounds.exists(i => i._2 <= i._1))
  }

  private[thylacine] override lazy val getValidated: UniformBeliefModel =
    if (validated) {
      this
    } else {
      UniformBeliefModel(upperBounds.getValidated, lowerBounds.getValidated, validated = true)
    }

  override val domainDimension: Int = upperBounds.dimension

  private[thylacine] lazy val negLogVolume: ResultOrErrIo[Double] =
    ResultOrErrIo.fromValue {
      -zippedBounds.map { case (lowerBound, upperBound) =>
        Math.log(upperBound - lowerBound)
      }.sum
    }

  private lazy val negInfinity: ResultOrErrIo[Double] =
    ResultOrErrIo.fromValue(Double.NegativeInfinity)

  private[thylacine] lazy val zeroVector: ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromValue(VectorContainer.zeros(domainDimension))

  private[thylacine] def insideBounds(input: VectorContainer): Boolean =
    zippedBounds.zip(input.scalaVector).forall {
      case ((lowerBound, upperBound), value) if value >= lowerBound && value < upperBound =>
        true
      case _ =>
        false
    }

  private[thylacine] override def logPdfAt(
      input: VectorContainer
  ): ResultOrErrIo[Double] =
    if (insideBounds(input)) {
      negLogVolume
    } else {
      negInfinity
    }

  //We artificially set the gradient here to guide optimisers and
  //samplers using gradient information
  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation {
      VectorContainer {
        zippedBounds.zip(input.scalaVector).map {
          case ((lowerBound, _), value) if value < lowerBound =>
            lowerBound - value
          case ((_, upperBound), value) if value > upperBound =>
            upperBound - value
          case _ =>
            0d
        }
      }
    }

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
