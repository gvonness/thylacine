package ai.entrolution
package thylacine.model.integration

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.computation.ResultOrErrF

import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] trait ModelParameterIntegrator[F[_]] {
  this: AsyncImplicits[F] =>

  protected def integrateOverModelParameters(
      integrand: BigDecimal => BigDecimal
  ): ResultOrErrF[F, BigDecimal]

  // BigDecimal is used as part of the public API, as these integrations
  // can cover a very large set of magnitudes
  final def integrate(integrand: BigDecimal => BigDecimal): F[BigDecimal] =
    for {
      integrationRes <- integrateOverModelParameters(integrand).value
      result <- integrationRes match {
                  case Right(res) =>
                    Async[F].pure(res)
                  case Left(erratum) =>
                    Async[F].raiseError(new RuntimeException(erratum.toString))
                }
    } yield result
}
