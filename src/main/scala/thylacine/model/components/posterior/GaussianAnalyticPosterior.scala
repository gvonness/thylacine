/*
 * Copyright 2020-2022 Greg von Nessi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.entrolution
package thylacine.model.components.posterior

import thylacine.model.components.likelihood._
import thylacine.model.components.posterior.GaussianAnalyticPosterior._
import thylacine.model.components.prior._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core._
import thylacine.model.core.computation.Erratum.UnexpectedErratum
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{MatrixContainer, VectorContainer}
import thylacine.model.sampling.ModelParameterSampler

import breeze.linalg._
import breeze.stats.distributions._
import cats.effect.kernel.Async
import cats.syntax.all._
import org.apache.commons.math3.random.MersenneTwister

import scala.{Vector => ScalaVector}

case class GaussianAnalyticPosterior[F[_]: Async](
    private[thylacine] override val priors: Set[GaussianPrior[F]],
    private[thylacine] override val likelihoods: Set[GaussianLinearLikelihood[F]],
    private[thylacine] override val validated: Boolean
) extends AsyncImplicits[F]
    with Posterior[F, GaussianPrior[F], GaussianLinearLikelihood[F]]
    with ModelParameterSampler[F]
    with CanValidate[GaussianAnalyticPosterior[F]] {
  if (!validated) {
    // Ensure there are no conflicting identifiers.
    assert(priors.size == priors.map(_.identifier).size)
    assert(
      likelihoods.size == likelihoods.map(_.posteriorTermIdentifier).size
    )
    assert(
      priors
        .map((i: GaussianPrior[F]) => i.posteriorTermIdentifier)
        .intersect(likelihoods.map(_.posteriorTermIdentifier))
        .isEmpty
    )
  }

  lazy val maxLogPdf: F[Double] = mean.flatMap(logPdfAt)

  private[thylacine] override lazy val getValidated: GaussianAnalyticPosterior[F] =
    if (validated) {
      this
    } else {
      GaussianAnalyticPosterior(
        priors = priors.map(_.getValidated),
        likelihoods = likelihoods.map(_.getValidated),
        validated = true
      )
    }

  private[thylacine] override val isAnalytic: Boolean = true

  private lazy val rawDistribution: ResultOrErrF[F, MultivariateGaussian] = {
    val priorsAdded: ResultOrErrF[F, AnalyticPosteriorAccumulation[F]] =
      priors.toVector
        .foldLeft(
          orderedParameterIdentifiersWithDimension.map { idPlusDim =>
            AnalyticPosteriorAccumulation(
              orderedParameterIdentifiersWithDimension = idPlusDim
            )
          }
        )((acc, p) => acc.flatMap(_.add(p)))

    val allAdded: ResultOrErrF[F, AnalyticPosteriorAccumulation[F]] =
      likelihoods.toVector
        .foldLeft(priorsAdded)((acc, l) => acc.flatMap(_.add(l)))

    allAdded.flatMap(_.gRawDistribution)
  }

  lazy val mean: F[Map[String, ScalaVector[Double]]] =
    (for {
      distribution <- rawDistribution
      result       <- rawVectorToModelParameterCollection(distribution.mean)
    } yield result.genericScalaRepresentation).liftToF

  // For testing only
  lazy val covarianceStridedVector: F[ScalaVector[Double]] =
    rawDistribution.map(_.covariance.toArray.toVector).liftToF

  private[thylacine] override def logPdfAt(
      input: ModelParameterCollection[F]
  ): ResultOrErrF[F, Double] =
    for {
      rawVector <- modelParameterCollectionToRawVector(input)
      result    <- rawDistribution.map(_.logPdf(rawVector))
    } yield result

  override protected def sampleModelParameters: ResultOrErrF[F, ModelParameterCollection[F]] =
    for {
      vectorContainer <- rawSampleModelParameters
      result          <- rawVectorToModelParameterCollection(vectorContainer.rawVector)
    } yield result

  override protected def rawSampleModelParameters: ResultOrErrF[F, VectorContainer] =
    rawDistribution.map(dist => VectorContainer(dist.sample()))

  def init: F[Unit] =
    sample.void
}

object GaussianAnalyticPosterior {

  def apply[F[_]: Async](
      priors: Set[GaussianPrior[F]],
      likelihoods: Set[GaussianLinearLikelihood[F]]
  ): GaussianAnalyticPosterior[F] =
    GaussianAnalyticPosterior(priors = priors, likelihoods = likelihoods, validated = false)

  // Contains the logic for merging and ordered set of
  // Gaussian Priors and Linear Likelihoods to produce a single
  // Multivariate Gaussian distribution that represents the
  // Posterior distribution
  private[thylacine] case class AnalyticPosteriorAccumulation[F[_]: Async](
      priorMean: Option[VectorContainer] = None,
      priorCovariance: Option[MatrixContainer] = None,
      data: Option[VectorContainer] = None,
      likelihoodCovariance: Option[MatrixContainer] = None,
      likelihoodTransformations: Option[MatrixContainer] = None,
      orderedParameterIdentifiersWithDimension: ScalaVector[(ModelParameterIdentifier, Int)]
  ) {

    private[thylacine] lazy val gRawDistribution: ResultOrErrF[F, MultivariateGaussian] =
      (for {
        pmContainer <- priorMean
        pcContainer <- priorCovariance
        dContainer  <- data
        lcContainer <- likelihoodCovariance
        tmContainer <- likelihoodTransformations
      } yield {
        val newInversePriorCovariance = inv(pcContainer.rawMatrix)
        val newInverseCovariance =
          newInversePriorCovariance + (tmContainer.rawMatrix.t * (lcContainer.rawMatrix \ tmContainer.rawMatrix))
        val newCovariance = inv(newInverseCovariance)

        // In reality, this is suffers from some pretty serious rounding errors
        // with all the multiple matrix inversions that need to happen
        val newMean =
          newInverseCovariance \ (pcContainer.rawMatrix \ pmContainer.rawVector +
            tmContainer.rawMatrix.t * (lcContainer.rawMatrix \ dContainer.rawVector))

        implicit val randBasis: RandBasis = new RandBasis(
          new ThreadLocalRandomGenerator(
            new MersenneTwister((newCovariance, newMean).hashCode())
          )
        )

        MultivariateGaussian(newMean, (newCovariance + newCovariance.t) * 0.5).toResultM
      }).getOrElse(
        UnexpectedErratum(
          "Can't create posterior Gaussian distribution: A term is missing"
        ).toResultM
      )

    private[thylacine] def add(
        prior: GaussianPrior[F]
    ): ResultOrErrF[F, AnalyticPosteriorAccumulation[F]] = {
      val incomingPriorMean       = prior.priorData.data
      val incomingPriorCovariance = prior.priorData.covariance
      this
        .copy(
          priorMean = Some(
            this.priorMean
              .map(_ rawConcatenateWith incomingPriorMean)
              .getOrElse(incomingPriorMean)
          ),
          priorCovariance = Some(
            this.priorCovariance
              .map(_ diagonalMergeWith incomingPriorCovariance)
              .getOrElse(incomingPriorCovariance)
          )
        )
        .toResultM
    }

    private[thylacine] def add(
        likelihood: GaussianLinearLikelihood[F]
    ): ResultOrErrF[F, AnalyticPosteriorAccumulation[F]] = {
      val incomingData           = likelihood.observations.data
      val incomingDataCovariance = likelihood.observations.covariance

      for {
        incomingTransformationMatrixCollection <-
          likelihood.forwardModel.getJacobian
        incomingTransformationMatrix <-
          orderedParameterIdentifiersWithDimension.map { id =>
            incomingTransformationMatrixCollection.index.getOrElse(
              id._1,
              MatrixContainer.zeros(likelihood.forwardModel.rangeDimension, id._2)
            )
          }.reduce(_ columnMergeWith _).toResultM
      } yield this.copy(
        data = Some(
          this.data
            .map(_ rawConcatenateWith incomingData)
            .getOrElse(incomingData)
        ),
        likelihoodCovariance = Some(
          this.likelihoodCovariance
            .map(
              _ diagonalMergeWith incomingDataCovariance
            )
            .getOrElse(incomingDataCovariance)
        ),
        likelihoodTransformations = Some(
          this.likelihoodTransformations
            .map(
              _ rowMergeWith incomingTransformationMatrix
            )
            .getOrElse(incomingTransformationMatrix)
        )
      )
    }

  }
}
