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
package thylacine.model.components.prior

import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.values.VectorContainer
import thylacine.model.distributions
import thylacine.model.distributions.UniformDistribution

import ai.entrolution.thylacine.model.core.computation.ResultOrErrF
import ai.entrolution.thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

case class UniformPrior(
    private[thylacine] override val identifier: ModelParameterIdentifier,
    private[thylacine] val maxBounds: Vector[Double],
    private[thylacine] val minBounds: Vector[Double],
    private[thylacine] override val validated: Boolean = false
) extends Prior[UniformDistribution] {

  protected override lazy val priorModel: UniformDistribution =
    distributions
      .UniformDistribution(upperBounds = VectorContainer(maxBounds), lowerBounds = VectorContainer(minBounds))
      .getValidated

  private[thylacine] override lazy val getValidated: UniformPrior =
    if (validated) this
    else this.copy(validated = true)

  private[thylacine] final override def pdfAt(
      input: ModelParameterCollection
  ): ResultOrErrF[Double] =
    for {
      vector <- input.retrieveIndex(identifier)
      result <- if (priorModel.insideBounds(vector)) {
                  priorModel.negLogVolume.map(Math.exp)
                } else {
                  ResultOrErrIo.fromValue(0d)
                }
    } yield result

  private[thylacine] final override def pdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    priorModel.zeroVector.map(IndexedVectorCollection(identifier, _))

  protected override def rawSampleModelParameters: ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation(priorModel.getRawSample)
}

object UniformPrior {

  def apply(
      label: String,
      maxBounds: Vector[Double],
      minBounds: Vector[Double]
  ): UniformPrior =
    UniformPrior(
      identifier = ModelParameterIdentifier(label),
      maxBounds = maxBounds,
      minBounds = minBounds
    )
}
