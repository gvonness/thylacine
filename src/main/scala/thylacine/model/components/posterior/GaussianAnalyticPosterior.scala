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
import cats.implicits._

case class GaussianAnalyticPosterior(
    priors: Set[GaussianPrior],
    likelihoods: Set[GaussianLinearLikelihood],
    validated: Boolean = false
) extends Posterior[GaussianPrior, GaussianLinearLikelihood]
    with CanValidate[GaussianAnalyticPosterior] {
  if (!validated) {
    // Ensure there are no conflicting identifiers.
    assert(priors.size == priors.map(_.identifier).size)
    assert(
      likelihoods.size == likelihoods.map(_.posteriorTermIdentifier).size
    )
    assert(
      priors.toList.toSet
        .map((i: GaussianPrior) => i.posteriorTermIdentifier)
        .intersect(likelihoods.map(_.posteriorTermIdentifier))
        .isEmpty
    )
  }

  override lazy val getValidated: GaussianAnalyticPosterior =
    if (validated) {
      this
    } else {
      GaussianAnalyticPosterior(
        priors = priors.map(_.getValidated),
        likelihoods = likelihoods.map(_.getValidated),
        validated = true
      )
    }

  override val isAnalytic: Boolean = true

  private lazy val rawDistribution: MultivariateGaussian = {
    val priorsAdded: ResultOrErrIo[AnalyticPosteriorAccumulation] =
      priors.toList
        .sortBy(_.posteriorTermIdentifier)
        .foldLeft(
          orderedParameterIdentifiersWithDimension.map { idPlusDim =>
            AnalyticPosteriorAccumulation(
              orderedParameterIdentifiersWithDimension = idPlusDim
            )
          }
        )((acc, p) => acc.flatMap(_.add(p)))

    val allAdded: ResultOrErrIo[AnalyticPosteriorAccumulation] =
      likelihoods.toList
        .sortBy(_.posteriorTermIdentifier)
        .foldLeft(priorsAdded)((acc, l) => acc.flatMap(_.add(l)))

    allAdded.flatMap(_.gRawDistribution).value.unsafeRunSync() match {
      case Right(dist) => dist
      case Left(err)   => throw new RuntimeException(err.message)
    }
  }

  lazy val mean: ResultOrErrIo[ModelParameterCollection] =
    for {
      mean   <- ResultOrErrIo.fromCalculation(rawDistribution.mean)
      result <- rawVectorToModelParameterCollection(mean)
    } yield result

  override def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrIo[Double] =
    for {
      rawVector <- modelParameterCollectionToRawVector(input)
      result    <- ResultOrErrIo.fromCalculation(rawDistribution.logPdf(rawVector))
    } yield result

  override def logPdfGradientAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    ResultOrErrIo.fromResultOrErrorIo {
      for {
        priorGradientsFiber <-
          priors.toList.parTraverse(_.logPdfGradientAt(input)).value.start
        likelihoodGradientsFiber <-
          likelihoods.toList.parTraverse(_.logPdfGradientAt(input)).value.start
        priorGradients      <- priorGradientsFiber.joinWithNever
        likelihoodGradients <- likelihoodGradientsFiber.joinWithNever
      } yield (priorGradients, likelihoodGradients) match {
        case (Right(pgs), Right(lgs)) =>
          Right(
            pgs.reduce(_ rawSumWith _) rawSumWith lgs.reduce(_ rawSumWith _)
          )
        case (Left(pe), Left(le)) =>
          Left(
            UnexpectedErratum(
              s"""Error encountered calculating logPdfGradient at $input:
               |Prior erratum: $pe
               |Likelihood erratum: $le
               |""".stripMargin
            )
          )
        case (Left(pe), _) =>
          Left(
            UnexpectedErratum(
              s"""Error encountered calculating logPdfGradient at $input:
               |Prior erratum: $pe
               |""".stripMargin
            )
          )
        case (_, Left(le)) =>
          Left(
            UnexpectedErratum(
              s"""Error encountered calculating logPdfGradient at $input:
               |Likelihood erratum: $le
               |""".stripMargin
            )
          )
      }
    }

  override def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    for {
      vectorContainer <- rawSampleModelParameters
      result          <- rawVectorToModelParameterCollection(vectorContainer.rawVector)
    } yield result

  override protected def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    for {
      result       <- ResultOrErrIo.fromCalculation(rawDistribution.sample())
      vectorResult <- ResultOrErrIo.fromCalculation(VectorContainer(result))
    } yield vectorResult
}

object GaussianAnalyticPosterior {

  // Contains the logic for merging and ordered set of
  // Gaussian Priors and Linear Likelihoods to produce a single
  // Multivariate Gaussian distribution that represents the
  // Posterior distribution
  case class AnalyticPosteriorAccumulation(
      priorMean: Option[VectorContainer] = None,
      priorCovariance: Option[MatrixContainer] = None,
      data: Option[VectorContainer] = None,
      likelihoodCovariance: Option[MatrixContainer] = None,
      likelihoodTransformations: Option[MatrixContainer] = None,
      orderedParameterIdentifiersWithDimension: List[
        (ModelParameterIdentifier, Int)
      ]
  ) {

    lazy val gRawDistribution: ResultOrErrIo[MultivariateGaussian] =
      (for {
        pmContainer <- priorMean
        pcContainer <- priorCovariance
        dContainer  <- data
        lcContainer <- likelihoodCovariance
        tmContainer <- likelihoodTransformations
      } yield {
        val newInversePriorCovariance = inv(pcContainer.rawMatrix)
        val newInverseLikelihoodCovarianceTerm =
          tmContainer.rawMatrix.t * inv(lcContainer.rawMatrix)
        val newCovariance =
          inv(
            newInversePriorCovariance + (newInverseLikelihoodCovarianceTerm * tmContainer.rawMatrix)
          )
        val newMean =
          newCovariance * (newInversePriorCovariance * pmContainer.rawVector + newInverseLikelihoodCovarianceTerm * dContainer.rawVector)
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

    def add(
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
              .map(_ rawDiagonalExtendWith incomingPriorCovariance)
              .getOrElse(incomingPriorCovariance)
          )
        )
      }
    }

    def add(
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
                MatrixContainer.zeros(likelihood.forwardModel.rangeDimension,
                                      id._2
                )
              )
            }.reduce(_ rawRowExtendWith _)
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
                            _ rawDiagonalExtendWith incomingDataCovariance
                          )
                          .getOrElse(incomingDataCovariance)
                      ),
                      likelihoodTransformations = Some(
                        this.likelihoodTransformations
                          .map(
                            _ rawColumnExtendWith incomingTransformationMatrix
                          )
                          .getOrElse(incomingTransformationMatrix)
                      )
                    )
                  }
      } yield result
    }

  }
}
