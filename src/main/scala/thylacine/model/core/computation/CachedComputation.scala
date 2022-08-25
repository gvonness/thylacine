package ai.entrolution
package thylacine.model.core.computation

import bengal.stm.STM
import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.core.computation.CachedComputation.{ComputationResult, getInMemoryKey}
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

case class CachedComputation[F[_]: STM: Async, T](
    computation: ModelParameterCollection[F] => T,
    cacheDepth: Option[Int] = None
)(scalarClock: TxnVar[F, Int], computationCache: TxnVarMap[F, Int, ComputationResult[T]])
    extends ComputationWrapper[F, T] {

  private val getTime: ResultOrErrF[F, Int] =
    scalarClock.get.commit.toResultM

  private val tickAndGet: ResultOrErrF[F, Int] =
    (for {
      _           <- scalarClock.modify(_ + 1)
      innerResult <- scalarClock.get
    } yield innerResult).commit.toResultM

  private def retrieveKeyFromComputationStore(
      key: Int
  ): ResultOrErrF[F, Option[T]] =
    computationCache
      .get(key)
      .map(_.map(_.result))
      .commit
      .toResultM

  private def updateAccessTimeForKeyInComputationStore(
      key: Int,
      time: Int
  ): ResultOrErrF[F, Unit] = {
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
      .flatMap(_ => Async[F].unit)
      .toResultM
  }

  private def upsertResultIntoComputationStore(
      key: Int,
      newResult: ComputationResult[T]
  ): ResultOrErrF[F, Unit] = {
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
      .flatMap(_ => Async[F].unit)
      .toResultM
  }

  private def cleanComputationStore(
      computationCache: TxnVarMap[F, Int, ComputationResult[T]]
  ): ResultOrErrF[F, Unit] =
    computationCache.modify { ec =>
      if (ec.size > cacheDepth.getOrElse(0)) {
        ec.toSeq
          .sortBy(_._2.lastAccessed)
          .takeRight(cacheDepth.getOrElse(0))
          .toMap
      } else {
        ec
      }
    }.commit.start
      .flatMap(_ => Async[F].unit)
      .toResultM

  private def retrieveComputationFromStoreFor(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, Option[T]] =
    for {
      time <- tickAndGet
      key  <- getInMemoryKey(input).toResultM
      ec   <- retrieveKeyFromComputationStore(key)
      _ <- ec match {
             case Some(_) =>
               updateAccessTimeForKeyInComputationStore(
                 key,
                 time
               )
             case _ =>
               ResultOrErrF.unit
           }
    } yield ec

  private def updateComputationStoreWith(input: ModelParameterCollection[F], result: T): ResultOrErrF[F, Unit] =
    for {
      time <- getTime
      key  <- getInMemoryKey(input).toResultM
      _    <- upsertResultIntoComputationStore(key, ComputationResult(result, time))
      _    <- cleanComputationStore(computationCache)
    } yield ()

  override private[stm] def performComputation(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, T] =
    if (cacheDepth.exists(_ > 0)) {
      for {
        oCachedResult <- retrieveComputationFromStoreFor(input)
        result <- oCachedResult.map(_.toResultM).getOrElse {
                    for {
                      innerResult <- computation(input).toResultM
                      _           <- updateComputationStoreWith(input, innerResult).start
                    } yield innerResult
                  }
      } yield result
    } else {
      computation(input).toResultM
    }
}

object CachedComputation {

  def applyF[F[_]: STM: Async, T](
      computation: ModelParameterCollection[F] => T,
      cacheDepth: Option[Int] = None
  ): F[CachedComputation[F, T]] =
    for {
      scalarClock      <- TxnVar.of(0)
      computationCache <- TxnVarMap.of(Map[Int, ComputationResult[T]]())
    } yield CachedComputation(
      computation = computation,
      cacheDepth = cacheDepth
    )(
      scalarClock = scalarClock,
      computationCache = computationCache
    )

  private[thylacine] def getInMemoryKey[F[_]: Async](input: ModelParameterCollection[F]): Int =
    input.hashCode()

  private[thylacine] case class ComputationResult[T](result: T, lastAccessed: Int) {

    private[thylacine] def updateLastAccess(accessedAt: Int): ComputationResult[T] =
      this.copy(lastAccessed = accessedAt)
  }

}
