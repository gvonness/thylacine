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

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

private[thylacine] trait Prior[+BM <: BeliefModel]
    extends ModelParameterPdf
    with PosteriorTerm
    with ModelParameterSampler
    with ModelParameterGenerator
    with CanValidate[Prior[_]] {

  protected def priorModel: BM

  final val label: String = identifier.value

  override final val posteriorTermIdentifier: TermIdentifier = TermIdentifier(
    identifier.value
  )

  override final val domainDimension: Int    = priorModel.domainDimension
  override final val generatorDimension: Int = priorModel.domainDimension

  override final def logPdfAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[Double] =
    for {
      vector <- input.retrieveIndex(identifier)
      res    <- priorModel.logPdfAt(vector)
    } yield res

  override final def logPdfGradientAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    for {
      vector  <- input.retrieveIndex(identifier)
      gradLog <- priorModel.logPdfGradientAt(vector)
      res <- ResultOrErrIo.fromCalculation(
               IndexedVectorCollection(identifier, gradLog)
             )
    } yield res

  override final def sampleModelParameters
      : ResultOrErrIo[ModelParameterCollection] =
    for {
      vectorResult <- rawSampleModelParameters
    } yield IndexedVectorCollection(identifier, vectorResult)
}
