package ai.entrolution
package thylacine.model.core.computation

import thylacine.model.core.computation.Erratum._

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.concurrent.{Future, TimeoutException}
import scala.util.{Failure, Success, Try}

private[thylacine] object ResultOrErrF {

  object Implicits {

    implicit class ToF[F[_]: Async, T](input: ResultOrErrF[F, T]) {

      lazy val liftToF: F[T] =
        input.value.flatMap {
          case Right(res) => Async[F].pure(res)
          case Left(err)  => Async[F].raiseError(new RuntimeException(err.message))
        }
    }

    implicit class FromResultOrErrorF[F[_], T](input: F[ResultOrErr[T]]) {

      lazy val toResultM: ResultOrErrF[F, T] =
        EitherT(input)
    }

    implicit class FromResultOrError[T](input: ResultOrErr[T]) {

      def toResultM[F[_]: Async]: ResultOrErrF[F, T] =
        Async[F].pure(input).toResultM
    }

    implicit class FromErratum(erratum: Erratum) {

      def toResultM[F[_]: Async, T]: ResultOrErrF[F, T] =
        Async[F]
          .pure[ResultOrErr[T]](Left(erratum))
          .toResultM
    }

    implicit class FromThrowable(ex: Throwable) {

      def toResultM[F[_]: Async, T]: ResultOrErrF[F, T] =
        ex match {
          case ex: TimeoutException =>
            TransientErratum(ex).toResultM
          case e: Throwable =>
            UnexpectedErratum(e).toResultM
        }
    }

    implicit class FromF[F[_]: Async, T](input: F[T]) {

      lazy val toResultM: ResultOrErrF[F, T] =
        input
          .map(Either.right[Erratum, T])
          .handleErrorWith(i => i.toResultM[F, T].value)
          .toResultM
    }

    implicit class FromCalculation[T](input: () => T) {

      def toResultM[F[_]: Async]: ResultOrErrF[F, T] =
        Async[F].delay(input()).toResultM
    }

    implicit class FromValue[T](input: T) {

      def toResultM[F[_]: Async]: ResultOrErrF[F, T] =
        (() => input).toResultM
    }

    implicit class FromFuture[T](input: () => Future[T]) {

      def toResultM[F[_]: Async]: ResultOrErrF[F, T] =
        Async[F].fromFuture(Async[F].delay(input())).toResultM
    }

    implicit class FromTryF[F[_]: Async, T](input: F[Try[T]]) {

      lazy val toResultM: ResultOrErrF[F, T] =
        input.flatMap {
          case Success(value)     => value.toResultM.value
          case Failure(exception) => exception.toResultM[F, T].value
        }.toResultM
    }
  }

  import Implicits._

  private[thylacine] def unit[F[_]: Async]: ResultOrErrF[F, Unit] =
    ().toResultM
}
