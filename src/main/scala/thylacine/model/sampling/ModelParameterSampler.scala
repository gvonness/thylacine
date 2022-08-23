package ai.entrolution
package thylacine.model.sampling

import thylacine.model.core.values.VectorContainer

import cats.effect.IO

private[thylacine] trait ModelParameterSampler {

  protected def sampleModelParameters: ResultOrErrIo[ModelParameterCollection]

  // Low-level API - For sampling priors
  protected def rawSampleModelParameters: ResultOrErrIo[VectorContainer]

  final def sample: IO[Map[String, Vector[Double]]] =
    for {
      sampleRes <- sampleModelParameters.value
      result <- sampleRes match {
                  case Right(res) =>
                    IO.pure(res)
                  case Left(erratum) =>
                    IO.raiseError(new RuntimeException(erratum.toString))
                }
    } yield result.genericScalaRepresentation
}
