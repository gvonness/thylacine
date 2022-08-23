package ai.entrolution
package thylacine.model.integration

import cats.effect.IO

private[thylacine] trait ModelParameterIntegrator {

  protected def integrateOverModelParameters(
      integrand: BigDecimal => BigDecimal
  ): ResultOrErrIo[BigDecimal]

  // BigDecimal is used as part of the public API, as these integrations
  // can cover a very large set of magnitudes
  final def integrate(integrand: BigDecimal => BigDecimal): IO[BigDecimal] =
    for {
      integrationRes <- integrateOverModelParameters(integrand).value
      result <- integrationRes match {
                  case Right(res) =>
                    IO.pure(res)
                  case Left(erratum) =>
                    IO.raiseError(new RuntimeException(erratum.toString))
                }
    } yield result
}
