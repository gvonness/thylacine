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

import thylacine.model.components.posterior.PosteriorTerm
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.{ ModelParameterContext, ModelParameterGenerator, ModelParameterPdf }
import thylacine.model.core.values.{ IndexedVectorCollection, VectorContainer }
import thylacine.model.distributions.Distribution
import thylacine.model.sampling.ModelParameterSampler

import cats.effect.kernel.Async
import cats.syntax.all._

trait Prior[F[_], +D <: Distribution]
    extends ModelParameterPdf[F]
    with PosteriorTerm
    with ModelParameterContext
    with ModelParameterSampler[F]
    with ModelParameterGenerator
    with CanValidate[Prior[F, _]] {
  this: AsyncImplicits[F] =>

  protected def priorDistribution: D

  final val label: String = identifier.value

  final override val posteriorTermIdentifier: TermIdentifier = TermIdentifier(
    identifier.value
  )

  final override val domainDimension: Int    = priorDistribution.domainDimension
  final override val generatorDimension: Int = priorDistribution.domainDimension

  final override def logPdfAt(
    input: IndexedVectorCollection
  ): F[Double] =
    Async[F].delay(priorDistribution.logPdfAt(input.retrieveIndex(identifier)))

  final override def logPdfGradientAt(
    input: IndexedVectorCollection
  ): F[ModelParameterCollection] =
    Async[F].delay {
      IndexedVectorCollection(
        identifier,
        priorDistribution
          .logPdfGradientAt(input.retrieveIndex(identifier))
      )
    }

  final override private[thylacine] lazy val orderedParameterIdentifiersWithDimension
    : Vector[(ModelParameterIdentifier, Int)] =
    Vector(
      ModelParameterIdentifier(
        posteriorTermIdentifier.value
      ) -> generatorDimension
    )

  private val sampleModelParameters: F[ModelParameterCollection] =
    rawSampleModelParameters.map(IndexedVectorCollection(identifier, _))

  final override def sampleModelParameters(numberOfSamples: Int): F[Set[ModelParameterCollection]] =
    (1 to numberOfSamples).toList.traverse(_ => sampleModelParameters).map(_.toSet)

  // testing
  private[thylacine] def rawLogPdfGradientAt(input: Vector[Double]): Vector[Double] =
    priorDistribution.logPdfGradientAt(VectorContainer(input)).scalaVector
}
