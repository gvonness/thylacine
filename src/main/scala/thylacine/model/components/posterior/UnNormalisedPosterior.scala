package ai.entrolution
package thylacine.model.components.posterior

import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.Erratum.ResultOrErrIo
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection

import cats.effect.implicits._
import cats.implicits._

case class UnNormalisedPosterior(
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]]
) extends NonAnalyticPosterior(priors, likelihoods) {

  override private[thylacine] def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      priorFib              <- priors.toList.traverse(_.logPdfAt(input)).start
      likelihoodFib         <- likelihoods.toList.traverse(_.logPdfAt(input)).start
      priorEvaluations      <- priorFib.joinWithNever
      likelihoodEvaluations <- likelihoodFib.joinWithNever
    } yield (priorEvaluations ++ likelihoodEvaluations).sum
}
