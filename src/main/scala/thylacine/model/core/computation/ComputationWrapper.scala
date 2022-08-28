package ai.entrolution
package thylacine.model.core.computation

import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

private[stm] trait ComputationWrapper[F[_], T] {
  private[stm] def performComputation(input: ModelParameterCollection[F]): ResultOrErrF[F, T]
}
