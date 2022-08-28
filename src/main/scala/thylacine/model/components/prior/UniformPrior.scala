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

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{IndexedVectorCollection, VectorContainer}
import thylacine.model.distributions
import thylacine.model.distributions.UniformDistribution

import cats.effect.kernel.Async

case class UniformPrior[F[_]: Async](
    private[thylacine] override val identifier: ModelParameterIdentifier,
    private[thylacine] val maxBounds: Vector[Double],
    private[thylacine] val minBounds: Vector[Double],
    private[thylacine] override val validated: Boolean = false
) extends AsyncImplicits[F]
    with Prior[F, UniformDistribution[F]] {

  protected override lazy val priorModel: UniformDistribution[F] =
    distributions
      .UniformDistribution(upperBounds = VectorContainer(maxBounds), lowerBounds = VectorContainer(minBounds))
      .getValidated

  private[thylacine] override lazy val getValidated: UniformPrior[F] =
    if (validated) this
    else this.copy(validated = true)

  private[thylacine] final override def pdfAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, Double] =
    for {
      vector <- input.retrieveIndex(identifier)
      result <- if (priorModel.insideBounds(vector)) {
                  priorModel.negLogVolume.map(Math.exp)
                } else {
                  0d.toResultM
                }
    } yield result

  private[thylacine] final override def pdfGradientAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, ModelParameterCollection[F]] =
    priorModel.zeroVector.map(IndexedVectorCollection(identifier, _))

  protected override def rawSampleModelParameters: ResultOrErrF[F, VectorContainer] =
    priorModel.getRawSample.toResultM
}

object UniformPrior {

  def apply[F[_]: Async](
      label: String,
      maxBounds: Vector[Double],
      minBounds: Vector[Double]
  ): UniformPrior[F] =
    UniformPrior(
      identifier = ModelParameterIdentifier(label),
      maxBounds = maxBounds,
      minBounds = minBounds
    )
}
