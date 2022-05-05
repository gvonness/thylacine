package ai.entrolution
package thylacine.model.core

import cats.data.EitherT
import cats.effect._

import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.{Failure, Success, Try}

sealed trait Erratum {
  def message: String

  override def toString: String =
    message

  def toSingleLineString: String =
    message.filter(_ >= ' ')
}

object Erratum {
  type ResultOrErr[T]   = Either[Erratum, T]
  type ResultOrErrIo[T] = EitherT[IO, Erratum, T]

  case class TransientErratum(message: String) extends Erratum

  object TransientErratum {

    def apply(ex: Throwable): TransientErratum =
      TransientErratum(s"""Unexpected transient exception encountered
                          |Exception message: ${ex.getMessage}
                          |Exception cause: ${ex.getCause}
                          |Exception stacktrace: ${ex.getStackTrace
        .mkString("Array(", ", ", ")")}""".stripMargin)
  }

  sealed trait PersistentErratum extends Erratum

  case class UnexpectedErratum(message: String) extends PersistentErratum

  object UnexpectedErratum {

    def apply(ex: Throwable): UnexpectedErratum =
      UnexpectedErratum(s"""Unexpected persistent exception encountered
                           |Exception message: ${ex.getMessage}
                           |Exception cause: ${ex.getCause}
                           |Exception stacktrace: ${ex.getStackTrace
        .mkString("Array(", ", ", ")")}""".stripMargin)
  }

  case class MinorErratum(message: String) extends PersistentErratum

  object ResultOrErrIo {

    def fromCalculation[T](calculation: => T): ResultOrErrIo[T] =
      fromIo(IO(calculation))

    def fromValue[T](value: T): ResultOrErrIo[T] =
      fromResultOrErrorIo(IO.pure[ResultOrErr[T]](Right(value)))

    def fromErratum[T](erratum: Erratum): ResultOrErrIo[T] =
      fromResultOrErrorIo(IO.pure[ResultOrErr[T]](Left(erratum)))

    def fromResultOrErr[T](resultOrErr: ResultOrErr[T]): ResultOrErrIo[T] =
      fromResultOrErrorIo(IO.pure(resultOrErr))

    def fromFuture[T](
        future: => Future[T]
    )(implicit ec: ExecutionContext): ResultOrErrIo[T] =
      fromResultOrErrorIo {
        IO.fromFuture(IO(future))
          .map(Right(_))
          .handleErrorWith(fromThrowable(_).value)
      }

    def fromThrowable[T](ex: Throwable): ResultOrErrIo[T] =
      ex match {
        case ex: TimeoutException =>
          fromErratum(TransientErratum(ex))
        case e: Throwable =>
          fromErratum(UnexpectedErratum(e))
      }

    def fromIo[T](spec: IO[T]): ResultOrErrIo[T] =
      fromResultOrErrorIo {
        spec
          .map(Right(_))
          .handleErrorWith(fromThrowable(_).value)
      }

    def fromTryIo[T](spec: IO[Try[T]]): ResultOrErrIo[T] =
      fromResultOrErrorIo {
        spec.flatMap {
          case Success(value)     => fromValue(value).value
          case Failure(exception) => fromThrowable(exception).value
        }
      }

    def fromResultOrErrorIo[T](
        resultOrErrorIo: IO[ResultOrErr[T]]
    ): ResultOrErrIo[T] =
      EitherT(resultOrErrorIo)

    val unit: ResultOrErrIo[Unit] =
      ResultOrErrIo.fromValue(())
  }
}
