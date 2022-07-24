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
package thylacine.model.components.likelihood

import thylacine.model.components.forwardmodel._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

private[thylacine] trait Likelihood[+FM <: ForwardModel, +BM <: BeliefModel]
    extends ModelParameterPdf
    with PosteriorTerm
    with CanValidate[Likelihood[_, _]] {

  private[thylacine] def observations: BelievedData
  private[thylacine] def observationModel: BM
  private[thylacine] def forwardModel: FM

  override final val domainDimension: Int =
    forwardModel.domainDimension

  private[thylacine] override final def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      mappedVec <- forwardModel.evalAt(input)
      res       <- observationModel.logPdfAt(mappedVec)
    } yield res

  private[thylacine] override final def logPdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    for {
      mappedVec  <- forwardModel.evalAt(input)
      forwardJac <- forwardModel.jacobianAt(input)
      measGrad   <- observationModel.logPdfGradientAt(mappedVec)
    } yield forwardJac.index.toList.map { fj =>
      IndexedVectorCollection(
        fj._1,
        VectorContainer(fj._2.rawMatrix.t * measGrad.rawVector)
      )
    }.reduce(_ rawMergeWith _)
}
