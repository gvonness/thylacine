package ai.entrolution
package thylacine.model.core.computation

import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

import cats.effect.kernel.Async

private[stm] abstract class ComputationWrapper[F[_]: Async, T] {
  private[stm] def performComputation(input: ModelParameterCollection[F]): ResultOrErrF[F, T]
}
