package ai.entrolution
package thylacine.model.optimization

import cats.effect.IO

private[thylacine] trait ModelParameterOptimizer {

  protected def calculateMaximumLogPdf(
      startingPt: ModelParameterCollection
  ): ResultOrErrIo[(Double, ModelParameterCollection)]

  final def findMaximumLogPdf(startPt: Map[String, Vector[Double]]): IO[(Double, Map[String, Vector[Double]])] =
    for {
      maximumResult <- calculateMaximumLogPdf(IndexedVectorCollection(startPt)).value
      result <- maximumResult match {
                  case Right(res) =>
                    IO.pure(res)
                  case Left(erratum) =>
                    IO.raiseError(new RuntimeException(erratum.toString))
                }
    } yield (result._1, result._2.genericScalaRepresentation)

  final def findMaximumLogPdf: IO[(Double, Map[String, Vector[Double]])] =
    findMaximumLogPdf(Map())

  final def findMaximumPdf(startPt: Map[String, Vector[Double]]): IO[(Double, Map[String, Vector[Double]])] =
    findMaximumLogPdf(startPt).map(i => (Math.exp(i._1), i._2))

  final def findMaximumPdf: IO[(Double, Map[String, Vector[Double]])] =
    findMaximumPdf(Map())
}
