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
import thylacine.model.components.posterior.PosteriorTerm
import thylacine.model.core._
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.ModelParameterPdf
import thylacine.model.core.values.{IndexedVectorCollection, VectorContainer}
import thylacine.model.distributions.Distribution

private[thylacine] trait Likelihood[F[_], +FM <: ForwardModel[F], +BM <: Distribution[F]]
    extends ModelParameterPdf[F]
    with PosteriorTerm
    with CanValidate[Likelihood[F, _, _]] {
  this: AsyncImplicits[F] =>

  private[thylacine] def observationModel: BM
  private[thylacine] def forwardModel: FM

  override final val domainDimension: Int =
    forwardModel.domainDimension

  private[thylacine] override final def logPdfAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, Double] =
    for {
      mappedVec <- forwardModel.evalAt(input)
      res       <- observationModel.logPdfAt(mappedVec)
    } yield res

  private[thylacine] override final def logPdfGradientAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, ModelParameterCollection[F]] =
    for {
      mappedVec  <- forwardModel.evalAt(input)
      forwardJac <- forwardModel.jacobianAt(input)
      measGrad   <- observationModel.logPdfGradientAt(mappedVec)
    } yield forwardJac.index.toList.map { fj =>
      IndexedVectorCollection(
        fj._1,
        VectorContainer((measGrad.rawVector.t * fj._2.rawMatrix).t)
      )
    }.reduce(_ rawMergeWith _)
}
