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

import bengal.stm._
import thylacine.model.components.forwardmodel.InMemoryMemoizedForwardModel._
import thylacine.model.core._
import thylacine.model.core.IndexedVectorCollection._

import ai.entrolution.bengal.stm.model.{TxnVar, _}
import ai.entrolution.thylacine.model.core.Erratum.ResultOrErrF.Implicits._
import bengal.stm.syntax.all._

import cats.effect.implicits._
import ai.entrolution.thylacine.model.core.Erratum.ResultOrErrF
import ai.entrolution.thylacine.model.core.computation.{CachedComputation, ResultOrErrF}
import ai.entrolution.thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import ai.entrolution.thylacine.model.core.values.{IndexedMatrixCollection, VectorContainer}
import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] abstract class InMemoryMemoizedForwardModel[F[_]: STM: Async](
    evalCache: CachedComputation[F, VectorContainer],
    jacobianCache: CachedComputation[F, IndexedMatrixCollection]
) extends ForwardModel {

  private[thylacine] override final def evalAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, VectorContainer] =
    evalCache.performComputation(input)

  private[thylacine] override final def jacobianAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, IndexedMatrixCollection] =
    if (cacheConfig.jacobianCacheEnabled) {
      for {
        oCachedResult <- retrieveJacobianFromStoreFor(input)
        result <- oCachedResult
                    .map(_.toResultM)
                    .getOrElse {
                      for {
                        innerResult <- computeJacobianAt(input)
                        _           <- updateJacobianStoreWith(input, innerResult)
                      } yield innerResult
                    }
      } yield result
    } else {
      computeJacobianAt(input)
    }
}

private[thylacine] object InMemoryMemoizedForwardModel {
//
//  private[thylacine] def getInMemoryKey(input: ModelParameterCollection): Int =
//    input.hashCode()
//
//  private[thylacine] case class ComputationResult[T](result: T, lastAccessed: Int) {
//
//    private[thylacine] def updateLastAccess(accessedAt: Int): ComputationResult[T] =
//      this.copy(lastAccessed = accessedAt)
//  }

  case class ForwardModelCachingConfig(evalCacheDepth: Option[Int], jacobianCacheDepth: Option[Int]) {
    lazy val evalCacheEnabled: Boolean     = evalCacheDepth.exists(_ > 0)
    lazy val jacobianCacheEnabled: Boolean = jacobianCacheDepth.exists(_ > 0)
  }

  object ForwardModelCachingConfig {
    val empty: ForwardModelCachingConfig = ForwardModelCachingConfig(None, None)
  }
}
