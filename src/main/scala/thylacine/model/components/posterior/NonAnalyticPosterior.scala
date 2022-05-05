package ai.entrolution
package thylacine.model.components.posterior

import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

import cats.effect.implicits._
import cats.implicits._

abstract class NonAnalyticPosterior(
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]]
) extends Posterior[Prior[_], Likelihood[_, _]] {
  override final val isAnalytic: Boolean = false

  override final def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      priorLogPdfSumFib <-
        priors.toList.parTraverse(_.logPdfAt(input)).map(_.sum).start
      likelihoodLogPdfSumFib <-
        likelihoods.toList.parTraverse(_.logPdfAt(input)).map(_.sum).start
      priorSum      <- priorLogPdfSumFib.joinWithNever
      likelihoodSum <- likelihoodLogPdfSumFib.joinWithNever
    } yield priorSum + likelihoodSum

  override final def logPdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    for {
      priorLogPdfGradSumFib <-
        priors.toList
          .parTraverse(_.logPdfGradientAt(input))
          .map(_.reduce(_ rawSumWith _))
          .start
      likelihoodLogPdfGradSumFib <-
        likelihoods.toList
          .parTraverse(_.logPdfGradientAt(input))
          .map(_.reduce(_ rawSumWith _))
          .start
      priorSum      <- priorLogPdfGradSumFib.joinWithNever
      likelihoodSum <- likelihoodLogPdfGradSumFib.joinWithNever
    } yield priorSum rawSumWith likelihoodSum
}
