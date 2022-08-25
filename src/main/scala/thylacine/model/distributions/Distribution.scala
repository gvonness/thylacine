package ai.entrolution
package thylacine.model.distributions

import thylacine.model.core.GenericScalarValuedMapping
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.values.VectorContainer

private[thylacine] trait Distribution[F[_]] extends GenericScalarValuedMapping {
  private[thylacine] def logPdfAt(input: VectorContainer): ResultOrErrF[F, Double]

  private[thylacine] def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrF[F, VectorContainer]
}
