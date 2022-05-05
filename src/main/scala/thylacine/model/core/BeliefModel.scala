package ai.entrolution
package thylacine.model.core

import thylacine.model.core.Erratum._

trait BeliefModel extends GenericScalarValuedMapping {
  def logPdfAt(input: VectorContainer): ResultOrErrIo[Double]

  def logPdfGradientAt(
      input: VectorContainer
  ): ResultOrErrIo[VectorContainer]
}
