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
import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

import breeze.linalg._
import breeze.stats.distributions.MultivariateGaussian
import cats.effect.unsafe.implicits.global

import scala.{Vector => ScalaVector}

case class GaussianAnalyticPosterior(
    priors: Set[GaussianPrior],
    likelihoods: Set[GaussianLinearLikelihood],
    validated: Boolean = false
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

    allAdded.flatMap(_.gRawDistribution).value.unsafeRunSync() match {
      case Right(dist) => dist
      case Left(err)   => throw new RuntimeException(err.message)
    }
  }

  private[thylacine] lazy val mean: ResultOrErrIo[ModelParameterCollection] =
    for {
      mean   <- ResultOrErrIo.fromCalculation(rawDistribution.mean)
      result <- rawVectorToModelParameterCollection(mean)
    } yield result

  private[thylacine] override def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      rawVector <- modelParameterCollectionToRawVector(input)
      result    <- ResultOrErrIo.fromCalculation(rawDistribution.logPdf(rawVector))
    } yield result

  override protected def sampleModelParameters
      : ResultOrErrIo[ModelParameterCollection] =
    for {
      vectorContainer <- rawSampleModelParameters
      result          <- rawVectorToModelParameterCollection(vectorContainer.rawVector)
    } yield result

  override protected def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation(VectorContainer(rawDistribution.sample()))
}

private[thylacine] object GaussianAnalyticPosterior {

  // Contains the logic for merging and ordered set of
  // Gaussian Priors and Linear Likelihoods to produce a single
  // Multivariate Gaussian distribution that represents the
  // Posterior distribution
  private[thylacine] case class AnalyticPosteriorAccumulation(
      vectorTerm: Option[DenseVector[Double]] = None,
      inverseCovariance: Option[DenseMatrix[Double]] = None,
      orderedParameterIdentifiersWithDimension: ScalaVector[
        (ModelParameterIdentifier, Int)
      ]
  ) {

    private[thylacine] lazy val gRawDistribution
        : ResultOrErrIo[MultivariateGaussian] =
      (for {
        vectorTerm            <- vectorTerm
        inverseCovarianceTerm <- inverseCovariance
      } yield {
        val newCovariance         = inv(inverseCovarianceTerm)
        val symmetrizedCovariance = (newCovariance + newCovariance.t) * 0.5
        val newMean               = symmetrizedCovariance * vectorTerm
        ResultOrErrIo.fromValue(
          MultivariateGaussian(newMean, symmetrizedCovariance)
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
      val priorCovarianceAddition =
        orderedParameterIdentifiersWithDimension.map { id =>
          if (prior.identifier == id._1) {
            prior.priorData.covariance
          } else {
            MatrixContainer.zeros(id._2, id._2)
          }
        }.reduce(_ diagonalMergeWith _)

      val newInvCov = inv(priorCovarianceAddition.rawMatrix)

      val priorMeanAddition = orderedParameterIdentifiersWithDimension.map {
        id =>
          if (prior.identifier == id._1) {
            prior.priorData.data
          } else {
            VectorContainer.zeros(id._2)
          }
      }.reduce(_ rawConcatenateWith _)

      val newVectorTerm = newInvCov * priorMeanAddition.rawVector

      ResultOrErrIo.fromCalculation {
        this.copy(
          vectorTerm = Some(
            this.vectorTerm.map(_ + newVectorTerm).getOrElse(newVectorTerm)
          ),
          inverseCovariance =
            Some(this.inverseCovariance.map(_ + newInvCov).getOrElse(newInvCov))
        )
      }
    }

    private[thylacine] def add(
        likelihood: GaussianLinearLikelihood
    ): ResultOrErrIo[AnalyticPosteriorAccumulation] = {
      val transformationMatrix =
        orderedParameterIdentifiersWithDimension.map { id =>
          likelihood.forwardModel.transform.index.getOrElse(
            id._1,
            MatrixContainer.zeros(likelihood.forwardModel.rangeDimension, id._2)
          )
        }.reduce(_ columnMergeWith _).rawMatrix

      val matrixTerm: DenseMatrix[Double] = transformationMatrix.t * inv(
        likelihood.observations.covariance.rawMatrix
      )

      val newInvCov: DenseMatrix[Double] = matrixTerm * transformationMatrix

      val newVectorTerm: DenseVector[Double] =
        matrixTerm * likelihood.observations.data.rawVector

      ResultOrErrIo.fromCalculation {
        this.copy(
          vectorTerm = Some(
            this.vectorTerm.map(_ + newVectorTerm).getOrElse(newVectorTerm)
          ),
          inverseCovariance =
            Some(this.inverseCovariance.map(_ + newInvCov).getOrElse(newInvCov))
        )
      }
    }

  }
}
