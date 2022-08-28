package ai.entrolution
package thylacine.model.optimization

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] trait ModelParameterOptimizer[F[_]] {
  this: AsyncImplicits[F] =>

  protected def calculateMaximumLogPdf(
      startingPt: ModelParameterCollection[F]
  ): ResultOrErrF[F, (Double, ModelParameterCollection[F])]

  final def findMaximumLogPdf(startPt: Map[String, Vector[Double]]): F[(Double, Map[String, Vector[Double]])] =
    for {
      maximumResult <- calculateMaximumLogPdf(IndexedVectorCollection(startPt)).value
      result <- maximumResult match {
                  case Right(res) =>
                    Async[F].pure(res)
                  case Left(erratum) =>
                    Async[F].raiseError(new RuntimeException(erratum.toString))
                }
    } yield (result._1, result._2.genericScalaRepresentation)

  final def findMaximumPdf(startPt: Map[String, Vector[Double]]): F[(Double, Map[String, Vector[Double]])] =
    findMaximumLogPdf(startPt).map(i => (Math.exp(i._1), i._2))
}
