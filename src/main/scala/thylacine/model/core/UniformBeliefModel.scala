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

import thylacine.model.core.Erratum._

import scala.collection.parallel.CollectionConverters._
import scala.{Vector => ScalaVector}

private[thylacine] case class UniformBeliefModel(
    maxBounds: VectorContainer,
    minBounds: VectorContainer,
    validated: Boolean = false
) extends BeliefModel
    with CanValidate[UniformBeliefModel] {

  private lazy val zippedBounds: ScalaVector[(Double, Double)] =
    minBounds.scalaVector.zip(maxBounds.scalaVector)

  if (!validated) {
    assert(maxBounds.dimension == minBounds.dimension)
    assert(!zippedBounds.exists(i => i._2 <= i._1))
  }

  private[thylacine] override lazy val getValidated: UniformBeliefModel =
    if (validated) {
      this
    } else {
      UniformBeliefModel(maxBounds.getValidated,
                         minBounds.getValidated,
                         validated = true
      )
    }

  private[thylacine] override val domainDimension: Int = maxBounds.dimension

  private[thylacine] lazy val negLogVolume: ResultOrErrIo[Double] =
    ResultOrErrIo.fromValue {
      -zippedBounds.map(zb => Math.log(zb._2 - zb._1)).sum
    }

  private lazy val negInfinity: ResultOrErrIo[Double] =
    ResultOrErrIo.fromValue(Double.NegativeInfinity)

  private[thylacine] lazy val zeroVector: ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromValue(VectorContainer.zeros(domainDimension))

  private[thylacine] def insideBounds(input: VectorContainer): Boolean =
    !zippedBounds.zip(input.scalaVector).exists { boundingVolumeTest =>
      boundingVolumeTest._2 < boundingVolumeTest._1._1 || boundingVolumeTest._2 >= boundingVolumeTest._1._2
    }

  private[thylacine] override def logPdfAt(
      input: VectorContainer
  ): ResultOrErrIo[Double] =
    if (insideBounds(input)) {
      negLogVolume
    } else {
      negInfinity
    }

  // Technically undefined (obviously) where the PDF is zero but this gets us the right semantics
  private[thylacine] override def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrIo[VectorContainer] =
    zeroVector

  private lazy val samplingScalingAndShift: ScalaVector[(Double, Double)] =
    zippedBounds.map(i => (i._2 - i._1, i._1))

  private[thylacine] def getRawSample: VectorContainer =
    VectorContainer(
      samplingScalingAndShift.par.map(i => Math.random() * i._1 + i._2).toVector
    )

}
