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
import thylacine.model.core.Erratum.{ResultOrErrIo, _}
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

import breeze.linalg._
import breeze.stats.distributions.{MultivariateGaussian, RandBasis, ThreadLocalRandomGenerator}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.apache.commons.math3.random.MersenneTwister

import scala.{Vector => ScalaVector}

case class GaussianAnalyticPosterior(
    private[thylacine] override val priors: Set[GaussianPrior],
    private[thylacine] override val likelihoods: Set[GaussianLinearLikelihood],
    private[thylacine] override val validated: Boolean
) extends Posterior[GaussianPrior, GaussianLinearLikelihood]
    with ModelParameterSampler
    with CanValidate[GaussianAnalyticPosterior] {
  if (!validated) {
    // Ensure there are no conflicting identifiers.
    assert(priors.size == priors.map(_.identifier).size)
    assert(
      likelihoods.size == likelihoods.map(_.posteriorTermIdentifier).size
    )
    assert(
      priors
        .map((i: GaussianPrior) => i.posteriorTermIdentifier)
        .intersect(likelihoods.map(_.posteriorTermIdentifier))
        .isEmpty
    )
  }

  lazy val maxLogPdf: IO[Double] = logPdfAt(mean)

  private[thylacine] override lazy val getValidated: GaussianAnalyticPosterior =
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

  private lazy val rawDistribution: MultivariateGaussian = {
    val priorsAdded: ResultOrErrIo[AnalyticPosteriorAccumulation] =
      priors.toVector
        .foldLeft(
          orderedParameterIdentifiersWithDimension.map { idPlusDim =>
            AnalyticPosteriorAccumulation(
              orderedParameterIdentifiersWithDimension = idPlusDim
            )
          }
        )((acc, p) => acc.flatMap(_.add(p)))

    val allAdded: ResultOrErrIo[AnalyticPosteriorAccumulation] =
      likelihoods.toVector
        .foldLeft(priorsAdded)((acc, l) => acc.flatMap(_.add(l)))

    ResultOrErrIo.materialize(allAdded.flatMap(_.gRawDistribution))
  }

  lazy val mean: Map[String, ScalaVector[Double]] =
    ResultOrErrIo.materialize {
      for {
        mean   <- ResultOrErrIo.fromCalculation(rawDistribution.mean)
        result <- rawVectorToModelParameterCollection(mean)
      } yield result.genericScalaRepresentation
    }

  // For testing only
  lazy val covarianceStridedVector: ScalaVector[Double] =
    rawDistribution.covariance.toArray.toVector

  private[thylacine] override def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      rawVector <- modelParameterCollectionToRawVector(input)
      result    <- ResultOrErrIo.fromCalculation(rawDistribution.logPdf(rawVector))
    } yield result

  override protected def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    for {
      vectorContainer <- rawSampleModelParameters
      result          <- rawVectorToModelParameterCollection(vectorContainer.rawVector)
    } yield result

  override protected def rawSampleModelParameters: ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation(VectorContainer(rawDistribution.sample()))

  def init(): IO[Unit] =
    sample.void
}

object GaussianAnalyticPosterior {

  def apply(
      priors: Set[GaussianPrior],
      likelihoods: Set[GaussianLinearLikelihood]
  ): GaussianAnalyticPosterior =
    GaussianAnalyticPosterior(priors = priors, likelihoods = likelihoods, validated = false)

  // Contains the logic for merging and ordered set of
  // Gaussian Priors and Linear Likelihoods to produce a single
  // Multivariate Gaussian distribution that represents the
  // Posterior distribution
  private[thylacine] case class AnalyticPosteriorAccumulation(
      priorMean: Option[VectorContainer] = None,
      priorCovariance: Option[MatrixContainer] = None,
      data: Option[VectorContainer] = None,
      likelihoodCovariance: Option[MatrixContainer] = None,
      likelihoodTransformations: Option[MatrixContainer] = None,
      orderedParameterIdentifiersWithDimension: ScalaVector[(ModelParameterIdentifier, Int)]
  ) {

    private[thylacine] lazy val gRawDistribution: ResultOrErrIo[MultivariateGaussian] =
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

        ResultOrErrIo.fromValue(
          MultivariateGaussian(newMean, (newCovariance + newCovariance.t) * 0.5)
        )
      }).getOrElse(
        ResultOrErrIo.fromErratum(
          UnexpectedErratum(
            "Can't create posterior Gaussian distribution: A term is missing"
          )
        )
      )

    private[thylacine] def add(
        prior: GaussianPrior
    ): ResultOrErrIo[AnalyticPosteriorAccumulation] = {
      val incomingPriorMean       = prior.priorData.data
      val incomingPriorCovariance = prior.priorData.covariance
      ResultOrErrIo.fromCalculation {
        this.copy(
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
    }

    private[thylacine] def add(
        likelihood: GaussianLinearLikelihood
    ): ResultOrErrIo[AnalyticPosteriorAccumulation] = {
      val incomingData           = likelihood.observations.data
      val incomingDataCovariance = likelihood.observations.covariance

      for {
        incomingTransformationMatrixCollection <-
          likelihood.forwardModel.getJacobian
        incomingTransformationMatrix <-
          ResultOrErrIo.fromCalculation {
            orderedParameterIdentifiersWithDimension.map { id =>
              incomingTransformationMatrixCollection.index.getOrElse(
                id._1,
                MatrixContainer.zeros(likelihood.forwardModel.rangeDimension, id._2)
              )
            }.reduce(_ columnMergeWith _)
          }
        result <- ResultOrErrIo.fromCalculation {
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
      } yield result
    }

  }
}
