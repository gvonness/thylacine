package ai.entrolution
package thylacine.model.components.forwardmodel

import thylacine.model.core._
import thylacine.model.core.Erratum._

trait ForwardModel extends GenericMapping {

  // Note that input validation should be done within
  // the surrounding likelihood
  def evalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer]

  // Note that input validation should be done within
  // the surrounding likelihood
  def jacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection]
}
