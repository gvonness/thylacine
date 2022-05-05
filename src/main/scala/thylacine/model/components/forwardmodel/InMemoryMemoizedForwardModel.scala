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
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

trait InMemoryMemoizedForwardModel extends MemoizedForwardModel {

  private val stm = STM.runtime[IO].unsafeRunSync()
  import stm._

  protected def maxResultsToCache: Int

  private val scalarClock: TxnVar[Int] =
    TxnVar.of(0).unsafeRunSync()

  private val tick: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo(scalarClock.modify(_ + 1).commit)

  private val getTime: ResultOrErrIo[Int] =
    ResultOrErrIo.fromIo(scalarClock.get.commit)

  private val evalCache: TxnVar[Map[Int, ComputationResult[VectorContainer]]] =
    TxnVar.of {
      Map[Int, ComputationResult[VectorContainer]]()
    }.unsafeRunSync()

  private val jacobianCache
      : TxnVar[Map[Int, ComputationResult[IndexedMatrixCollection]]] =
    TxnVar.of {
      Map[Int, ComputationResult[IndexedMatrixCollection]]()
    }.unsafeRunSync()

  private def retrieveKeyFromStore[T](
      key: Int,
      store: TxnVar[Map[Int, ComputationResult[T]]]
  ): ResultOrErrIo[Option[T]] =
    ResultOrErrIo.fromIo(
      store.get.map(_.get(key).map(_.result)).commit
    )

  private def updateAccessTimeForKeyInStore[T](
      key: Int,
      time: Int,
      store: TxnVar[Map[Int, ComputationResult[T]]]
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- store.modify { ec =>
               ec.get(key) match {
                 case Some(res) if time > res.lastAccessed =>
                   ec + (key -> res.updateLastAccess(time))
                 case _ => ()
               }
               ec
             }.commit.start
      } yield ()
    }

  private def upsertResultIntoStore[T](
      key: Int,
      newResult: ComputationResult[T],
      store: TxnVar[Map[Int, ComputationResult[T]]]
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- store.modify { ec =>
               ec.get(key) match {
                 case None =>
                   ec + (key -> newResult)
                 case Some(oldResult)
                     if newResult.lastAccessed > oldResult.lastAccessed =>
                   ec + (key -> newResult)
                 case _ =>
                   ()
               }
               ec
             }.commit.start
      } yield ()
    }

  private def cleanStore[T](
      store: TxnVar[Map[Int, ComputationResult[T]]]
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- store.modify { ec =>
               if (ec.size > maxResultsToCache) {
                 ec.toSeq
                   .sortBy(_._2.lastAccessed)
                   .takeRight(maxResultsToCache)
                   .toMap
               } else {
                 ec
               }
             }.commit.start
      } yield ()
    }

  private def retrieveFromStoreFor[T](
      input: ModelParameterCollection,
      store: TxnVar[Map[Int, ComputationResult[T]]]
  ): ResultOrErrIo[Option[T]] =
    for {
      _    <- tick
      time <- getTime
      key  <- getInMemoryKey(input)
      ec   <- retrieveKeyFromStore(key, store)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInStore(key, time, store)
             case _ =>
               ResultOrErrIo.unit
           }
    } yield ec

  final override protected def retrieveEvalFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[VectorContainer]] =
    retrieveFromStoreFor(input, evalCache)

  final override protected def retrieveJacobianFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[IndexedMatrixCollection]] =
    retrieveFromStoreFor(input, jacobianCache)

  private def updateStoreWith[T](
      input: ModelParameterCollection,
      eval: T,
      store: TxnVar[Map[Int, ComputationResult[T]]]
  ): ResultOrErrIo[Unit] =
    for {
      time <- getTime
      key  <- getInMemoryKey(input)
      _    <- upsertResultIntoStore(key, ComputationResult(eval, time), store)
      _    <- cleanStore(store)
    } yield ()

  final override protected def updateEvalStoreWith(
      input: ModelParameterCollection,
      eval: VectorContainer
  ): ResultOrErrIo[Unit] =
    updateStoreWith(input, eval, evalCache)

  final override protected def updateJacobianStoreWith(
      input: ModelParameterCollection,
      jacobian: IndexedMatrixCollection
  ): ResultOrErrIo[Unit] =
    updateStoreWith(input, jacobian, jacobianCache)
}

object InMemoryMemoizedForwardModel {

  def getInMemoryKey(input: ModelParameterCollection): ResultOrErrIo[Int] =
    ResultOrErrIo.fromCalculation(input.hashCode())

  case class ComputationResult[T](result: T, lastAccessed: Int) {

    def updateLastAccess(accessedAt: Int): ComputationResult[T] =
      this.copy(lastAccessed = accessedAt)
  }
}
