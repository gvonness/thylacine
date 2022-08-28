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
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.{ModelParameterGenerator, ModelParameterPdf}
import thylacine.model.distributions.Distribution
import thylacine.model.sampling.ModelParameterSampler

private[thylacine] trait Prior[F[_], +BM <: Distribution[F]]
    extends ModelParameterPdf[F]
    with PosteriorTerm
    with ModelParameterSampler[F]
    with ModelParameterGenerator
    with CanValidate[Prior[F, _]] {
  this: AsyncImplicits[F] =>

  protected def priorModel: BM

  final val label: String = identifier.value

  override final val posteriorTermIdentifier: TermIdentifier = TermIdentifier(
    identifier.value
  )

  override final val domainDimension: Int    = priorModel.domainDimension
  override final val generatorDimension: Int = priorModel.domainDimension

  override final def logPdfAt(
      input: IndexedVectorCollection[F]
  ): ResultOrErrF[F, Double] =
    for {
      vector <- input.retrieveIndex(identifier)
      res    <- priorModel.logPdfAt(vector)
    } yield res

  override final def logPdfGradientAt(
      input: IndexedVectorCollection[F]
  ): ResultOrErrF[F, ModelParameterCollection[F]] =
    for {
      vector  <- input.retrieveIndex(identifier)
      gradLog <- priorModel.logPdfGradientAt(vector)
      res     <- IndexedVectorCollection[F](identifier, gradLog).toResultM
    } yield res

  override final def sampleModelParameters: ResultOrErrF[F, ModelParameterCollection[F]] =
    rawSampleModelParameters.map(IndexedVectorCollection(identifier, _))
}
