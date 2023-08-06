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
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{ MatrixContainer, VectorContainer }
import thylacine.model.sampling.ModelParameterSampler

import breeze.linalg._
import breeze.stats.distributions._
import cats.effect.kernel.Async
import cats.syntax.all._
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.unused
import scala.{ Vector => ScalaVector }

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

  private[thylacine] override lazy val getValidated: GaussianAnalyticPosterior[F] =
    if (validated) {
      this
    } else {
      GaussianAnalyticPosterior(
        priors      = priors.map(_.getValidated),
        likelihoods = likelihoods.map(_.getValidated),
        validated   = true
      )
    }

  @unused
  lazy val maxLogPdf: F[Double] = logPdfAt(mean)

  lazy val entropy: Double = rawDistribution.entropy

  @unused
  lazy val priorEntropies: Set[Double] = priors.map(_.entropy)

  private lazy val rawDistribution: MultivariateGaussian = {
    val priorsAdded: AnalyticPosteriorAccumulation[F] =
      priors.toVector
        .foldLeft(
          AnalyticPosteriorAccumulation[F](
            orderedParameterIdentifiersWithDimension = orderedParameterIdentifiersWithDimension
          )
        )((acc, p) => acc.add(p))

    val allAdded: AnalyticPosteriorAccumulation[F] =
      likelihoods.toVector
        .foldLeft(priorsAdded)((acc, l) => acc.add(l))

    allAdded.gRawDistribution
  }

  lazy val mean: Map[String, ScalaVector[Double]] =
    rawVectorToModelParameterCollection(rawDistribution.mean).genericScalaRepresentation

  // For testing only
  lazy val covarianceStridedVector: ScalaVector[Double] =
    rawDistribution.covariance.toArray.toVector

  private[thylacine] override def logPdfAt(
    input: ModelParameterCollection
  ): F[Double] =
    Async[F].delay(rawDistribution.logPdf(modelParameterCollectionToRawVector(input)))

  override protected def rawSampleModelParameters: F[VectorContainer] =
    Async[F].delay(VectorContainer(rawDistribution.sample()))

  private def sampleModelParameters: F[ModelParameterCollection] =
    rawSampleModelParameters.map(s => rawVectorToModelParameterCollection(s.rawVector))
  protected override def sampleModelParameters(numberOfSamples: Int): F[Set[ModelParameterCollection]] =
    (1 to numberOfSamples).toList.traverse(_ => sampleModelParameters).map(_.toSet)

  def init: F[Unit] =
    sample(1).void
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
    priorMean: Option[VectorContainer]                 = None,
    priorCovariance: Option[MatrixContainer]           = None,
    data: Option[VectorContainer]                      = None,
    likelihoodCovariance: Option[MatrixContainer]      = None,
    likelihoodTransformations: Option[MatrixContainer] = None,
    orderedParameterIdentifiersWithDimension: ScalaVector[(ModelParameterIdentifier, Int)]
  ) {

    private[thylacine] lazy val gRawDistribution: MultivariateGaussian =
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

        MultivariateGaussian(newMean, (newCovariance + newCovariance.t) * 0.5)
      }).getOrElse(
        throw new RuntimeException("Can't create posterior Gaussian distribution: A term is missing")
      )

    private[thylacine] def add(
      prior: GaussianPrior[F]
    ): AnalyticPosteriorAccumulation[F] = {
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
    }

    private[thylacine] def add(
      likelihood: GaussianLinearLikelihood[F]
    ): AnalyticPosteriorAccumulation[F] = {
      val incomingData           = likelihood.observations.data
      val incomingDataCovariance = likelihood.observations.covariance
      val incomingTransformationMatrix = orderedParameterIdentifiersWithDimension
        .map { id =>
          likelihood.forwardModel.getJacobian.index.getOrElse(
            id._1,
            MatrixContainer.zeros(likelihood.forwardModel.rangeDimension, id._2)
          )
        }
        .reduce(_ columnMergeWith _)

      this.copy(
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
