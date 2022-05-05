package ai.entrolution
package thylacine.model.components.forwardmodel

import thylacine.model.core._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

// For all but the simplest inferences, the majority of computation
// will be tied up in forward model, and as such posterior
// operations may be greatly sped up with memoizing the associated
// computations. However, the effectiveness of this caching will
// be highly dependent on the inference itself (forward models,
// sampling strategy, model parameter scaling, etc.)
trait MemoizedForwardModel extends ForwardModel {

  protected def retrieveEvalFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[VectorContainer]]

  protected def retrieveJacobianFromStoreFor(
      input: ModelParameterCollection
  ): ResultOrErrIo[Option[IndexedMatrixCollection]]

  protected def updateEvalStoreWith(
      input: ModelParameterCollection,
      eval: VectorContainer
  ): ResultOrErrIo[Unit]

  protected def updateJacobianStoreWith(
      input: ModelParameterCollection,
      jacobian: IndexedMatrixCollection
  ): ResultOrErrIo[Unit]

  protected def computeEvalAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[VectorContainer]

  protected def computeJacobianAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[IndexedMatrixCollection]

  override final def evalAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[VectorContainer] =
    for {
      oCachedResult <- retrieveEvalFromStoreFor(input)
      result <- oCachedResult.map { res =>
                  ResultOrErrIo.fromValue(res)
                }.getOrElse {
                  for {
                    innerResult <- computeEvalAt(input)
                    _           <- updateEvalStoreWith(input, innerResult)
                  } yield innerResult
                }
    } yield result

  override final def jacobianAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[IndexedMatrixCollection] =
    for {
      oCachedResult <- retrieveJacobianFromStoreFor(input)
      result <- oCachedResult.map { res =>
                  ResultOrErrIo.fromValue(res)
                }.getOrElse {
                  for {
                    innerResult <- computeJacobianAt(input)
                    _           <- updateJacobianStoreWith(input, innerResult)
                  } yield innerResult
                }
    } yield result
}
