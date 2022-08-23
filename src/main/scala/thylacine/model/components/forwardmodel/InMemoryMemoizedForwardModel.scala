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
import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] trait InMemoryMemoizedForwardModel extends ForwardModel {

  private def scalarClockF[F[_]: STM: Async]: F[TxnVar[F, Int]] =
    TxnVar.of(0)

  private def evalCacheF[F[_]: STM: Async]: F[TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]] =
    TxnVar.of {
      Map[Int, ComputationResult[VectorContainer]]()
    }

  private def jacobianCacheF[F[_]: STM: Async]: F[TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]] =
    TxnVar.of {
      Map[Int, ComputationResult[IndexedMatrixCollection]]()
    }

  private def getTime[F[_]: STM: Async](scalarClock: TxnVar[F, Int]): ResultOrErrF[F, Int] =
    scalarClock.get.commit.toResultM

  private def tickAndGet[F[_]: STM: Async](scalarClock: TxnVar[F, Int]): ResultOrErrF[F, Int] =
    (for {
      _           <- scalarClock.modify(_ + 1)
      innerResult <- scalarClock.get
    } yield innerResult).commit.toResultM

  private def retrieveKeyFromEvalStore[F[_]: STM: Async](
      key: Int
  )(evalCache: TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]): ResultOrErrF[F, Option[VectorContainer]] =
    evalCache.get
      .map(_.get(key).map(_.result))
      .commit
      .toResultM

  private def retrieveKeyFromJacobianStore[F[_]: STM: Async](
      key: Int
  )(
      jacobianCache: TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]
  ): ResultOrErrF[F, Option[IndexedMatrixCollection]] =
    jacobianCache.get
      .map(_.get(key).map(_.result))
      .commit
      .toResultM

  private def updateAccessTimeForKeyInEvalStore[F[_]: STM: Async](
      key: Int,
      time: Int
  )(evalCache: TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]): ResultOrErrF[F, Unit] =
    evalCache.modify { ec =>
      ec.get(key) match {
        case Some(res) if time > res.lastAccessed =>
          ec + (key -> res.updateLastAccess(time))
        case _ => ()
      }
      ec
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  private def updateAccessTimeForKeyInJacobianStore[F[_]: STM: Async](
      key: Int,
      time: Int
  )(
      jacobianCache: TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]
  ): ResultOrErrF[F, Unit] =
    jacobianCache.modify { ec =>
      ec.get(key) match {
        case Some(res) if time > res.lastAccessed =>
          ec + (key -> res.updateLastAccess(time))
        case _ => ()
      }
      ec
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  private def upsertResultIntoEvalStore[F[_]: STM: Async](
      key: Int,
      newResult: ComputationResult[VectorContainer]
  )(evalCache: TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]): ResultOrErrF[F, Unit] =
    evalCache.modify { ec =>
      ec.get(key) match {
        case None =>
          ec + (key -> newResult)
        case Some(oldResult) if newResult.lastAccessed > oldResult.lastAccessed =>
          ec + (key -> newResult)
        case _ =>
          ec
      }
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  private def upsertResultIntoJacobianStore[F[_]: STM: Async](
      key: Int,
      newResult: ComputationResult[IndexedMatrixCollection]
  )(
      jacobianCache: TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]
  ): ResultOrErrF[F, Unit] =
    jacobianCache.modify { ec =>
      ec.get(key) match {
        case None =>
          ec + (key -> newResult)
        case Some(oldResult) if newResult.lastAccessed > oldResult.lastAccessed =>
          ec + (key -> newResult)
        case _ =>
          ec
      }
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  private def cleanEvalStore[F[_]: STM: Async](
      evalCache: TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]
  ): ResultOrErrF[F, Unit] =
    evalCache.modify { ec =>
      if (ec.size > cacheConfig.evalCacheDepth.getOrElse(0)) {
        ec.toSeq
          .sortBy(_._2.lastAccessed)
          .takeRight(cacheConfig.evalCacheDepth.getOrElse(0))
          .toMap
      } else {
        ec
      }
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  private def cleanJacobianStore[F[_]: STM: Async](
      jacobianCache: TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]
  ): ResultOrErrF[F, Unit] =
    jacobianCache.modify { ec =>
      if (ec.size > cacheConfig.jacobianCacheDepth.getOrElse(0)) {
        ec.toSeq
          .sortBy(_._2.lastAccessed)
          .takeRight(cacheConfig.jacobianCacheDepth.getOrElse(0))
          .toMap
      } else {
        ec
      }
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  override protected def retrieveEvalFromStoreFor[F[_]: STM: Async](
      input: ModelParameterCollection
  )(
      scalarClock: TxnVar[F, Int],
      evalCache: TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]
  ): ResultOrErrF[F, Option[VectorContainer]] =
    for {
      time <- tickAndGet(scalarClock)
      key  <- getInMemoryKey(input).toResultM
      ec   <- retrieveKeyFromEvalStore(key)(evalCache)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInEvalStore(
                 key,
                 time
               )(evalCache)
             case _ =>
               ResultOrErrF.unit
           }
    } yield ec

  override protected def retrieveJacobianFromStoreFor[F[_]: STM: Async](
      input: ModelParameterCollection
  )(
      scalarClock: TxnVar[F, Int],
      jacobianCache: TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]
  ): ResultOrErrF[F, Option[IndexedMatrixCollection]] =
    for {
      time <- tickAndGet(scalarClock)
      key  <- getInMemoryKey(input).toResultM
      ec   <- retrieveKeyFromJacobianStore(key)(jacobianCache)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInJacobianStore(
                 key,
                 time
               )(jacobianCache)
             case _ =>
               ResultOrErrF.unit
           }
    } yield ec

  override protected def updateEvalStoreWith[F[_]: STM: Async](
      input: ModelParameterCollection,
      eval: VectorContainer
  )(
      scalarClock: TxnVar[F, Int],
      evalCache: TxnVar[F, Map[Int, ComputationResult[VectorContainer]]]
  ): ResultOrErrF[F, Unit] =
    for {
      time <- getTime(scalarClock)
      key  <- getInMemoryKey(input).toResultM
      _    <- upsertResultIntoEvalStore(key, ComputationResult(eval, time))(evalCache)
      _    <- cleanEvalStore(evalCache)
    } yield ()

  override protected def updateJacobianStoreWith[F[_]: STM: Async](
      input: ModelParameterCollection,
      jacobian: IndexedMatrixCollection
  )(
      scalarClock: TxnVar[F, Int],
      jacobianCache: TxnVar[F, Map[Int, ComputationResult[IndexedMatrixCollection]]]
  ): ResultOrErrF[F, Unit] =
    for {
      time <- getTime(scalarClock)
      key  <- getInMemoryKey(input).toResultM
      _    <- upsertResultIntoJacobianStore(key, ComputationResult(jacobian, time))(jacobianCache)
      _    <- cleanJacobianStore(jacobianCache)
    } yield ()

  private[thylacine] override final def evalAt[F[_] : STM : Async](
                                                                    input: ModelParameterCollection
                                                                  ): ResultOrErrF[F, VectorContainer] =
    if (cacheConfig.evalCacheEnabled) {
      for {
        oCachedResult <- retrieveEvalFromStoreFor(input)
        result <- oCachedResult.map(_.toResultM).getOrElse {
          for {
            innerResult <- computeEvalAt(input)
            _ <- updateEvalStoreWith(input, innerResult)
          } yield innerResult
        }
      } yield result
    } else {
      computeEvalAt(input)
    }

  private[thylacine] override final def jacobianAt[F[_] : STM : Async](
                                                                        input: ModelParameterCollection
                                                                      ): ResultOrErrF[F, IndexedMatrixCollection] =
    if (cacheConfig.jacobianCacheEnabled) {
      for {
        oCachedResult <- retrieveJacobianFromStoreFor(input)
        result <- oCachedResult
          .map(_.toResultM)
          .getOrElse {
            for {
              innerResult <- computeJacobianAt(input)
              _ <- updateJacobianStoreWith(input, innerResult)
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
