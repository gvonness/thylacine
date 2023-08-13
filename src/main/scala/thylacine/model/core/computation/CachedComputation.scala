/*
 * Copyright 2023 Greg von Nessi
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
package thylacine.model.core.computation

import bengal.stm.STM
import bengal.stm.model.*
import bengal.stm.syntax.all.*
import thylacine.model.core.computation.CachedComputation.ComputationResult
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.syntax.all.*

import scala.annotation.unused

case class CachedComputation[F[_]: STM: Async, T](
  computation: ModelParameterCollection => T,
  cacheDepth: Option[Int] = None
)(scalarClock: TxnVar[F, Int], computationCache: TxnVarMap[F, Int, ComputationResult[T]])
    extends ComputationWrapper[F, T] {

  private val getTime: F[Int] =
    scalarClock.get.commit

  private val tickAndGet: F[Int] =
    (for {
      _           <- scalarClock.modify(_ + 1)
      innerResult <- scalarClock.get
    } yield innerResult).commit

  private def retrieveKeyFromComputationStore(
    key: Int
  ): F[Option[T]] =
    computationCache
      .get(key)
      .map(_.map(_.result))
      .commit

  private def updateAccessTimeForKeyInComputationStore(
    key: Int,
    time: Int
  ): F[Unit] = {
    def updateAccessTime(input: ComputationResult[T]): ComputationResult[T] =
      if (time > input.lastAccessed) {
        input.updateLastAccess(time)
      } else {
        input
      }

    computationCache
      .modify(key, updateAccessTime)
      .handleErrorWith(_ => STM[F].unit)
      .commit
      .start
      .void
  }

  private def upsertResultIntoComputationStore(
    key: Int,
    newResult: ComputationResult[T]
  ): F[Unit] = {
    def updateResult(input: ComputationResult[T]): ComputationResult[T] =
      if (newResult.lastAccessed > input.lastAccessed) {
        newResult
      } else {
        input
      }

    computationCache
      .modify(key, updateResult)
      .handleErrorWith(_ => computationCache.set(key, newResult))
      .commit
      .start
      .void
  }

  private val cleanComputationStore: F[Unit] =
    computationCache
      .modify { ec =>
        if (ec.size > cacheDepth.getOrElse(0)) {
          ec.toSeq
            .sortBy(_._2.lastAccessed)
            .takeRight(cacheDepth.getOrElse(0))
            .toMap
        } else {
          ec
        }
      }
      .commit
      .start
      .void

  private def retrieveComputationFromStoreFor(
    input: ModelParameterCollection
  ): F[Option[T]] =
    for {
      time <- tickAndGet
      key  <- Async[F].delay(input.intHash)
      ec   <- retrieveKeyFromComputationStore(key)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInComputationStore(
                 key,
                 time
               )
             case _ =>
               Async[F].unit
           }
    } yield ec

  private def updateComputationStoreWith(input: ModelParameterCollection, result: T): F[Unit] =
    for {
      time <- getTime
      key  <- Async[F].delay(input.intHash)
      _    <- upsertResultIntoComputationStore(key, ComputationResult(result, time))
      _    <- cleanComputationStore
    } yield ()

  override private[thylacine] def performComputation(
    input: ModelParameterCollection
  ): F[T] =
    if (cacheDepth.exists(_ > 0)) {
      for {
        oCachedResult <- retrieveComputationFromStoreFor(input)
        result <- oCachedResult
                    .map(Async[F].pure)
                    .getOrElse {
                      for {
                        innerResult <- Async[F].delay(computation(input))
                        _           <- updateComputationStoreWith(input, innerResult).start
                      } yield innerResult
                    }
      } yield result
    } else {
      Async[F].delay(computation(input))
    }
}

object CachedComputation {

  @unused
  def of[F[_]: STM: Async, T](
    computation: ModelParameterCollection => T,
    cacheDepth: Option[Int] = None
  ): F[CachedComputation[F, T]] =
    for {
      scalarClock      <- TxnVar.of(0)
      computationCache <- TxnVarMap.of(Map[Int, ComputationResult[T]]())
    } yield CachedComputation(
      computation = computation,
      cacheDepth  = cacheDepth
    )(
      scalarClock      = scalarClock,
      computationCache = computationCache
    )

  private[thylacine] case class ComputationResult[T](result: T, lastAccessed: Int) {

    private[thylacine] def updateLastAccess(accessedAt: Int): ComputationResult[T] =
      this.copy(lastAccessed = accessedAt)
  }

}
