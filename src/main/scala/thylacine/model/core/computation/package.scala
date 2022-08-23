package ai.entrolution
package thylacine.model.core

import cats.data.EitherT

package object computation {
  private[thylacine] type ResultOrErr[T]        = Either[Erratum, T]
  private[thylacine] type ResultOrErrF[F[_], T] = EitherT[F, Erratum, T]
}
