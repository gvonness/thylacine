package ai.entrolution
package thylacine.model.core

import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

trait ModelParameterSampleGenerator {
  def sampleModelParameters: ResultOrErrIo[ModelParameterCollection]

  // Low-level API
  protected def rawSampleModelParameters: ResultOrErrIo[VectorContainer]
}
