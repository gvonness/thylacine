package ai.entrolution
package thylacine.model.sampling

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.VectorContainer

import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] trait ModelParameterSampler[F[_]] {
  this: AsyncImplicits[F] =>
  protected def sampleModelParameters: ResultOrErrF[F, ModelParameterCollection[F]]

  // Low-level API - For sampling priors
  protected def rawSampleModelParameters: ResultOrErrF[F, VectorContainer]

  final def sample: F[Map[String, Vector[Double]]] =
    for {
      sampleRes <- sampleModelParameters.value
      result <- sampleRes match {
                  case Right(res) =>
                    Async[F].pure(res)
                  case Left(erratum) =>
                    Async[F].raiseError(new RuntimeException(erratum.toString))
                }
    } yield result.genericScalaRepresentation
}
