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
package thylacine.model.components.forwardmodel

import thylacine.model.core._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

// For all but the simplest inferences, the majority of computation
// will be tied up in forward model, and as such posterior
// operations may be greatly sped up with memoizing the associated
// computations. However, the effectiveness of this caching will
// be highly dependent on the inference itself (forward models,
// sampling strategy, model parameter scaling, etc.)
private[thylacine] trait MemoizedForwardModel extends ForwardModel {

  protected def retrieveEvalFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[VectorContainer]]

  protected def retrieveJacobianFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[IndexedMatrixCollection]]

  protected def updateEvalStoreWith(
      input: ModelParameterCollection,
      eval: VectorContainer
  ): ResultOrErrIo[Unit]

  protected def updateJacobianStoreWith(
      input: ModelParameterCollection,
      jacobian: IndexedMatrixCollection
  ): ResultOrErrIo[Unit]

  protected def computeEvalAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[VectorContainer]

  protected def computeJacobianAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[IndexedMatrixCollection]

  private[thylacine] override final def evalAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[VectorContainer] =
    for {
      oCachedResult <- retrieveEvalFromStoreFor(input)
      result <- oCachedResult.map { res =>
                  ResultOrErrIo.fromValue(res)
                }.getOrElse {
                  for {
                    innerResult <- computeEvalAt(input)
                    _           <- updateEvalStoreWith(input, innerResult)
                  } yield innerResult
                }
    } yield result

  private[thylacine] override final def jacobianAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[IndexedMatrixCollection] =
    for {
      oCachedResult <- retrieveJacobianFromStoreFor(input)
      result <- oCachedResult.map { res =>
                  ResultOrErrIo.fromValue(res)
                }.getOrElse {
                  for {
                    innerResult <- computeJacobianAt(input)
                    _           <- updateJacobianStoreWith(input, innerResult)
                  } yield innerResult
                }
    } yield result
}
