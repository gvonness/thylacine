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

private[thylacine] abstract class InMemoryMemoizedForwardModel(implicit val stm: STM[IO]) extends MemoizedForwardModel {

  import stm._

  private val scalarClock: TxnVar[Int] =
    TxnVar.of(0).unsafeRunSync()

  private val getTime: ResultOrErrIo[Int] =
    ResultOrErrIo.fromIo(scalarClock.get.commit)

  private val tickAndGet: ResultOrErrIo[Int] =
    ResultOrErrIo.fromIo((scalarClock.modify(_ + 1) >> scalarClock.get).commit)

  private val evalCache: TxnVar[Map[Int, ComputationResult[VectorContainer]]] =
    TxnVar.of {
      Map[Int, ComputationResult[VectorContainer]]()
    }.unsafeRunSync()

  private val jacobianCache: TxnVar[Map[Int, ComputationResult[IndexedMatrixCollection]]] =
    TxnVar.of {
      Map[Int, ComputationResult[IndexedMatrixCollection]]()
    }.unsafeRunSync()

  private def retrieveKeyFromEvalStore(
      key: Int
  ): ResultOrErrIo[Option[VectorContainer]] =
    ResultOrErrIo.fromIo(
      evalCache.get.map(_.get(key).map(_.result)).commit
    )

  private def retrieveKeyFromJacobianStore(
      key: Int
  ): ResultOrErrIo[Option[IndexedMatrixCollection]] =
    ResultOrErrIo.fromIo(
      jacobianCache.get.map(_.get(key).map(_.result)).commit
    )

  private def updateAccessTimeForKeyInEvalStore(
      key: Int,
      time: Int
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- evalCache.modify { ec =>
               ec.get(key) match {
                 case Some(res) if time > res.lastAccessed =>
                   ec + (key -> res.updateLastAccess(time))
                 case _ => ()
               }
               ec
             }.commit.start
      } yield ()
    }

  private def updateAccessTimeForKeyInJacobianStore(
      key: Int,
      time: Int
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- jacobianCache.modify { ec =>
               ec.get(key) match {
                 case Some(res) if time > res.lastAccessed =>
                   ec + (key -> res.updateLastAccess(time))
                 case _ => ()
               }
               ec
             }.commit.start
      } yield ()
    }

  private def upsertResultIntoEvalStore(
      key: Int,
      newResult: ComputationResult[VectorContainer]
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- evalCache.modify { ec =>
               ec.get(key) match {
                 case None =>
                   ec + (key -> newResult)
                 case Some(oldResult) if newResult.lastAccessed > oldResult.lastAccessed =>
                   ec + (key -> newResult)
                 case _ =>
                   ec
               }
             }.commit.start
      } yield ()
    }

  private def upsertResultIntoJacobianStore(
      key: Int,
      newResult: ComputationResult[IndexedMatrixCollection]
  ): ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- jacobianCache.modify { ec =>
               ec.get(key) match {
                 case None =>
                   ec + (key -> newResult)
                 case Some(oldResult) if newResult.lastAccessed > oldResult.lastAccessed =>
                   ec + (key -> newResult)
                 case _ =>
                   ec
               }
             }.commit.start
      } yield ()
    }

  private def cleanEvalStore: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- evalCache.modify { ec =>
               if (ec.size > cacheConfig.evalCacheDepth.getOrElse(0)) {
                 ec.toSeq
                   .sortBy(_._2.lastAccessed)
                   .takeRight(cacheConfig.evalCacheDepth.getOrElse(0))
                   .toMap
               } else {
                 ec
               }
             }.commit.start
      } yield ()
    }

  private def cleanJacobianStore: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- jacobianCache.modify { ec =>
               if (ec.size > cacheConfig.jacobianCacheDepth.getOrElse(0)) {
                 ec.toSeq
                   .sortBy(_._2.lastAccessed)
                   .takeRight(cacheConfig.jacobianCacheDepth.getOrElse(0))
                   .toMap
               } else {
                 ec
               }
             }.commit.start
      } yield ()
    }

  override protected def retrieveEvalFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[VectorContainer]] =
    for {
      time <- tickAndGet
      key  <- ResultOrErrIo.fromCalculation(getInMemoryKey(input))
      ec   <- retrieveKeyFromEvalStore(key)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInEvalStore(
                 key,
                 time
               )
             case _ =>
               ResultOrErrIo.unit
           }
    } yield ec

  override protected def retrieveJacobianFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[IndexedMatrixCollection]] =
    for {
      time <- tickAndGet
      key  <- ResultOrErrIo.fromCalculation(getInMemoryKey(input))
      ec   <- retrieveKeyFromJacobianStore(key)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInJacobianStore(
                 key,
                 time
               )
             case _ =>
               ResultOrErrIo.unit
           }
    } yield ec

  override protected def updateEvalStoreWith(
      input: ModelParameterCollection,
      eval: VectorContainer
  ): ResultOrErrIo[Unit] =
    for {
      time <- getTime
      key  <- ResultOrErrIo.fromCalculation(getInMemoryKey(input))
      _    <- upsertResultIntoEvalStore(key, ComputationResult(eval, time))
      _    <- cleanEvalStore
    } yield ()

  override protected def updateJacobianStoreWith(
      input: ModelParameterCollection,
      jacobian: IndexedMatrixCollection
  ): ResultOrErrIo[Unit] =
    for {
      time <- getTime
      key  <- ResultOrErrIo.fromCalculation(getInMemoryKey(input))
      _    <- upsertResultIntoJacobianStore(key, ComputationResult(jacobian, time))
      _    <- cleanJacobianStore
    } yield ()
}

private[thylacine] object InMemoryMemoizedForwardModel {

  private[thylacine] def getInMemoryKey(input: ModelParameterCollection): Int =
    input.hashCode()

  private[thylacine] case class ComputationResult[T](result: T, lastAccessed: Int) {

    private[thylacine] def updateLastAccess(accessedAt: Int): ComputationResult[T] =
      this.copy(lastAccessed = accessedAt)
  }

  case class ForwardModelCachingConfig(evalCacheDepth: Option[Int], jacobianCacheDepth: Option[Int]) {
    lazy val evalCacheEnabled: Boolean     = evalCacheDepth.exists(_ > 0)
    lazy val jacobianCacheEnabled: Boolean = jacobianCacheDepth.exists(_ > 0)
  }

  object ForwardModelCachingConfig {
    val empty: ForwardModelCachingConfig = ForwardModelCachingConfig(None, None)
  }
}
