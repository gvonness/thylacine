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
package thylacine.model.core

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.concurrent.{Future, TimeoutException}
import scala.util.{Failure, Success, Try}

private[thylacine] sealed trait Erratum {
  private[thylacine] def message: String

  override def toString: String =
    message

  private[thylacine] def toSingleLineString: String =
    message.filter(_ >= ' ')
}

private[thylacine] object Erratum {
  private[thylacine] type ResultOrErr[T]        = Either[Erratum, T]
  private[thylacine] type ResultOrErrF[F[_], T] = EitherT[F, Erratum, T]

  private[thylacine] case class TransientErratum(message: String) extends Erratum

  private[thylacine] object TransientErratum {

    private[thylacine] def apply(ex: Throwable): TransientErratum =
      TransientErratum(s"""Unexpected transient exception encountered
                          |Exception message: ${ex.getMessage}
                          |Exception cause: ${ex.getCause}
                          |Exception stacktrace: ${ex.getStackTrace
        .mkString("Array(", ", ", ")")}""".stripMargin)
  }

  private[thylacine] sealed trait PersistentErratum extends Erratum

  private[thylacine] case class UnexpectedErratum(message: String) extends PersistentErratum

  private[thylacine] object UnexpectedErratum {

    private[thylacine] def apply(ex: Throwable): UnexpectedErratum =
      UnexpectedErratum(s"""Unexpected persistent exception encountered
                           |Exception message: ${ex.getMessage}
                           |Exception cause: ${ex.getCause}
                           |Exception stacktrace: ${ex.getStackTrace
        .mkString("Array(", ", ", ")")}""".stripMargin)
  }

  private[thylacine] case class MinorErratum(message: String) extends PersistentErratum

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
          Async[F].pure(input).toResultM
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
}
