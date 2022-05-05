package ai.entrolution
package thylacine.model.core

import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

trait ModelParameterPdf extends GenericScalarValuedMapping {

  def logPdfAt(input: ModelParameterCollection): ResultOrErrIo[Double]

  final def pdfAt(input: ModelParameterCollection): ResultOrErrIo[Double] =
    logPdfAt(input).map(Math.exp)

  def logPdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[ModelParameterCollection]

  final def pdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    for {
      pdf      <- pdfAt(input)
      gradLogs <- logPdfGradientAt(input)
    } yield gradLogs.index.toList.map { gl =>
      IndexedVectorCollection(gl._1, VectorContainer(pdf * gl._2.rawVector))
    }.reduce(_ rawMergeWith _)
}
