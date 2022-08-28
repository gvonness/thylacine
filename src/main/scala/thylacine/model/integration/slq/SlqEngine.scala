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
package thylacine.model.integration.slq

import bengal.stm.STM
import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.components.posterior._
import thylacine.model.components.prior._
import thylacine.model.core._
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.VectorContainer
import thylacine.model.integration.ModelParameterIntegrator
import thylacine.model.integration.slq.SamplingSimulation._
import thylacine.model.sampling.ModelParameterSampler

import breeze.linalg.DenseVector
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

/** Implementation of the SLQ (pronounced like "slick") algorithm
  * introduced in Appendix B of [3].
  *
  * While an evolution of Skilling's Nested Sampling technique
  * (see chapter 9 in [1] for an introduction or the Appendix
  * in [2] for a more mathematical analysis of the approach), this
  * algorithm still generally suffers from excessively high rejection
  * rates in high-dimensional inferences. Attempts to alleviate this issue
  * result in posterior integrations and sample simulations that are
  * typically far-too concentrated around the PDF maxima.
  *
  * TL;DR - Expect this algorithm to take a very long time to build its
  * quadratures for high-dimensional problems.
  *
  * [1] Sivia D and Skilling J
  *     Data Analysis: A Bayesian Tutorial
  *     Second Edition (2006) Oxford University Press
  * [2] von Nessi G T, Hole M J and The MAST Team
  *     A unified method for inference of tokamak equilibria and validation
  *       of force-balance models based on Bayesian analysis
  *     J. Phys. A.: Math. Theor. 46 (2013) 185501
  * [3] von Nessi G T, Hole M J and The MAST Team
  *     Recent developments in Bayesian inference of tokamak plasma
  *       equilibria and high-dimensional stochastic quadratures
  *     Plasma Phys. Control Fusion 56 (2014) 114011
  */
private[thylacine] trait SlqEngine[F[_]] extends ModelParameterIntegrator[F] with ModelParameterSampler[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  import SlqEngine._

  /*
   * - - -- --- ----- -------- -------------
   * Configuration
   * - - -- --- ----- -------- -------------
   */

  protected def slqSamplePoolSize: Int
  protected def slqNumberOfAbscissa: Int
  protected def slqScalingIncrement: Double
  protected def slqNominalAcceptanceRatio: Double
  protected def slqSampleParallelism: Int
  protected def seeds: Set[ModelParameterCollection[F]]

  protected def slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit
  protected def domainRebuildStartCallback: Unit => Unit
  protected def domainRebuildFinishCallback: Unit => Unit

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  protected val sampleDomain: TxnVar[F, PointInCubeCollection[F]]

  protected val samplePool: TxnVarMap[F, Double, ModelParameterCollection[F]]

  protected val samplePoolMinimumLogPdf: TxnVar[F, Double]

  protected val logPdfResults: TxnVar[F, Vector[(Double, ModelParameterCollection[F])]]

  protected val sampleDomainScalingState: TxnVar[F, QuadratureDomainTelemetry]

  protected val workTokenPool: TxnVar[F, Int]

  protected val abscissas: TxnVar[F, QuadratureAbscissaCollection]

  protected val quadratureIntegrations: TxnVar[F, QuadratureIntegrator[F]]

  protected val samplingSimulation: TxnVar[F, SamplingSimulation[F]]

  protected val isConverged: TxnVar[F, Boolean]

  /*
   * - - -- --- ----- -------- -------------
   * Sampling calculation
   * - - -- --- ----- -------- -------------
   */

  private def jitter(logPdf: Double): Txn[Double] = {
    val jitterResult =
      Math.nextAfter(logPdf, logPdf + (if (Math.random() >= 0.5) 1 else -1))

    samplePool
      .get(logPdf)
      .flatMap {
        case Some(_) => jitter(jitterResult)
        case _       => STM[F].pure(logPdf)
      }
      .handleErrorWith(_ => STM[F].pure(logPdf))
  }

  private def recordMinimumLogPdf(
      minLogPdf: Option[Double] = None
  ): Txn[Unit] =
    for {
      currentMinimum <- minLogPdf match {
                          case Some(res) =>
                            STM[F].pure(res)
                          case _ => samplePoolMinimumLogPdf.get
                        }
      currentMinValue <- samplePool.get(currentMinimum)
      _               <- STM[F].waitFor(currentMinValue.isDefined)
      _ <- currentMinValue match {
             case Some(value) =>
               for {
                 _ <- logPdfResults.modify {
                        (currentMinimum, value) +: _
                      }
                 _       <- samplePool.remove(currentMinimum)
                 samples <- samplePool.get
                 newMinimum <- if (samples.nonEmpty) {
                                 STM[F].delay(samples.keys.min)
                               } else {
                                 STM[F].pure(Double.MinValue)
                               }
                 _ <- samplePoolMinimumLogPdf.set(newMinimum)
               } yield ()
             case _ =>
               STM[F].abort(
                 new RuntimeException(
                   s"LogPdf of $currentMinimum not found in sample pool!"
                 )
               )
           }
      _ <- sampleDomainScalingState.modify(_.addAcceptance)
      _ <- abscissas.modify(_.extendAllAbscissaByOne)
      abscissaSize <-
        abscissas.get.map(_.size)
      _ <- if (abscissaSize > 2) {
             for {
               quadratures <-
                 abscissas.get.map(_.getQuadratures)
               logPdfs <-
                 logPdfResults.get.map(_.map(_._1))
               _ <-
                 quadratureIntegrations.set(
                   QuadratureIntegrator(logPdfs, quadratures)
                 )
             } yield ()
           } else STM[F].unit
    } yield ()

  private def drainSamplePool: F[Unit] =
    (for {
      samplePoolNonEmpty <- samplePool.get.map(_.nonEmpty)
      result <- if (samplePoolNonEmpty) {
                  recordMinimumLogPdf() >>
                    STM[F].pure(true)
                } else {
                  STM[F].pure(false)
                }
    } yield result).commit.flatMap(continue => if (continue) drainSamplePool else Async[F].unit)

  private def updateStateWithSampleCalculation(
      logPdf: Double,
      modelParameters: ModelParameterCollection[F]
  ): Txn[Boolean] =
    for {
      currentMinimum <- samplePoolMinimumLogPdf.get
      outerResult <- if (currentMinimum < logPdf) {
                       for {
                         jitteredLogPdf <- jitter(logPdf)
                         _              <- samplePool.set(jitteredLogPdf, modelParameters)
                         _              <- recordMinimumLogPdf(Some(currentMinimum))
                       } yield true
                     } else {
                       sampleDomainScalingState.modify(_.addRejection) >> STM[F].pure(false)
                     }
    } yield outerResult

  private lazy val sampleAndAnalyze: ResultOrErrF[F, Unit] =
    for {
      domain                 <- sampleDomain.get.commit.toResultM
      scalingFactorTelemetry <- sampleDomainScalingState.get.commit.toResultM
      sampleContainer        <- domain.getSample(scalingFactorTelemetry.currentScaleFactor)
      sample                 <- rawVectorToModelParameterCollection(sampleContainer.rawVector)
      logPdf                 <- logPdfAt(sample)
      result                 <- updateStateWithSampleCalculation(logPdf, sample).commit.toResultM
      _ <-
        if (
          result || (scalingFactorTelemetry.rejectionStreak > 0 && scalingFactorTelemetry.rejectionStreak % 100 == 0)
        ) {
          for {
            _ <- if (result) setConverged() else ResultOrErrF.unit
            telemetry <- (for {
                           samples      <- samplePool.get
                           integrations <- quadratureIntegrations.get
                         } yield (samples, scalingFactorTelemetry, integrations)).commit.toResultM
            negEntStats <- telemetry._3.negativeEntropyStats
          } yield slqTelemetryUpdateCallback(
            SlqTelemetryUpdate(
              negEntropyAvg = negEntStats.sum.toDouble / negEntStats.size,
              logPdf = logPdf,
              samplePoolMinimumLogPdf = telemetry._1.keySet.min,
              domainVolumeScaling = telemetry._2.currentScaleFactor,
              acceptancesSinceDomainRebuild = telemetry._2.acceptancesSinceLastRebuild,
              samplePoolSize = telemetry._1.size,
              domainCubeCount = domain.pointsInCube.size,
              iterationCount = telemetry._3.logPdfs.size
            )
          )
        } else {
          ResultOrErrF.unit
        }
    } yield ()

  private def sampleAndReplaceToken: ResultOrErrF[F, Unit] =
    (for {
      result <- sampleAndAnalyze.value
      _      <- workTokenPool.modify(_ + 1).commit
    } yield result).toResultM

  private def getWorkToken: Txn[Unit] =
    for {
      tokenAmount <- workTokenPool.get
      _           <- STM[F].waitFor(tokenAmount > 0)
      _           <- workTokenPool.set(tokenAmount - 1)
    } yield ()

  private def setConverged(): ResultOrErrF[F, Unit] = {
    def testConverged(
        iterationCount: Int,
        numberSamplePoints: Int,
        negativeEntropyStats: Vector[BigDecimal]
    ): Boolean =
      if (negativeEntropyStats.nonEmpty) {
        iterationCount > 1000 &&
        iterationCount >= 10 * numberSamplePoints * negativeEntropyStats.max
      } else {
        false
      }

    for {
      telemetry <- (for {
                     quadratures    <- quadratureIntegrations.get
                     iterationCount <- logPdfResults.get.map(_.size)
                     domainExhausted <-
                       sampleDomainScalingState.get.map(_.isConverged)
                   } yield (quadratures, iterationCount, domainExhausted)).commit.toResultM
      negEntStats <- telemetry._1.negativeEntropyStats
      _ <- isConverged
             .set(
               testConverged(telemetry._2, slqSamplePoolSize, negEntStats) || telemetry._3
             )
             .commit
             .toResultM
    } yield ()
  }

  private def initiateSampling: F[Boolean] =
    for {
      _                 <- getWorkToken.commit
      _                 <- sampleAndReplaceToken.value.start
      continueIteration <- isConverged.get.map(!_).commit
    } yield continueIteration

  private def samplingRecursion: F[Unit] =
    initiateSampling.flatMap(continueIteration =>
      if (continueIteration) {
        samplingRecursion
      } else {
        drainSamplePool >>
          (for {
            abscissasRaw <- abscissas.get.map(_.getAbscissas)
            logPdfRes    <- logPdfResults.get
            _ <-
              samplingSimulation.set(
                SamplingSimulationConstructed(logPdfRes, abscissasRaw)
              )
          } yield ()).commit
      }
    )

  /*
   * - - -- --- ----- -------- -------------
   * Sample domain management
   * - - -- --- ----- -------- -------------
   */

  private def getSamplesForDomainRebuild: ResultOrErrF[F, Boolean] =
    (for {
      samplingDomainState <- sampleDomainScalingState.get
      converged           <- isConverged.get
      _                   <- STM[F].waitFor(samplingDomainState.initiateRebuild || converged)
    } yield converged).commit.toResultM

  private def rebuildDomain: ResultOrErrF[F, Boolean] =
    for {
      converged <- getSamplesForDomainRebuild
      continueIteration <- if (!converged) {
                             for {
                               _       <- domainRebuildStartCallback.toResultM
                               samples <- samplePool.get.commit.toResultM
                               picCollection <-
                                 getPointInCubeCollection(
                                   samples.values.toVector,
                                   modelParameterCollectionToRawVector
                                 )
                               _ <- (for {
                                      _ <- sampleDomain.set(
                                             picCollection
                                           )
                                      _ <-
                                        sampleDomainScalingState
                                          .modify(
                                            _.resetForRebuild
                                          )
                                    } yield ()).commit.toResultM
                               _                 <- setConverged()
                               continueIteration <- isConverged.get.map(!_).commit.toResultM
                               _                 <- domainRebuildFinishCallback.toResultM
                             } yield continueIteration
                           } else {
                             false.toResultM
                           }
    } yield continueIteration

  private def domainRebuildRecursion: F[Unit] =
    rebuildDomain.value.flatMap {
      case Right(false) => Async[F].unit
      case _            => domainRebuildRecursion
    }

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private lazy val initialiseTvars: ResultOrErrF[F, Unit] =
    (for {
      _ <- sampleDomainScalingState.set {
             QuadratureDomainTelemetry(
               currentScaleFactor = 1.0,
               acceptances = 0,
               rejections = 0,
               nominalAcceptance = slqNominalAcceptanceRatio,
               minValue = slqScalingIncrement,
               acceptancesSinceLastRebuild = 0,
               rejectionStreak = 0
             )
           }
      _ <- workTokenPool.set(slqSampleParallelism)
      _ <-
        abscissas.set(
          QuadratureAbscissaCollection(slqNumberOfAbscissa, slqSamplePoolSize)
        )
      _ <- logPdfResults.set(Vector())
      _ <- quadratureIntegrations.set(QuadratureIntegrator.empty)
      _ <- samplingSimulation.set(SamplingSimulation.empty)
      _ <- isConverged.set(false)
    } yield ()).commit.toResultM

  private def analyseSeeds: ResultOrErrF[F, Map[Double, ModelParameterCollection[F]]] =
    for {
      result <- seeds.toVector.parTraverse { s =>
                  logPdfAt(s).map(i => (i, s))
                }
    } yield result.toMap

  private def getInitialSample(
      logPdfs: Set[Double]
  ): ResultOrErrF[F, (Double, ModelParameterCollection[F])] =
    (for {
      sample <- samplePriors
      logPdf <- logPdfAt(sample)
    } yield (logPdf, sample)).flatMap { i =>
      if (logPdfs.contains(i._1)) {
        getInitialSample(logPdfs)
      } else {
        i.toResultM
      }
    }

  private def initialise: ResultOrErrF[F, Unit] =
    for {
      _     <- initialiseTvars
      seeds <- analyseSeeds
      logPdfPriorResults <-
        (1 to Math.max(1, slqSamplePoolSize - seeds.size)).foldLeft(
          seeds.toResultM
        ) { (i, _) =>
          for {
            prev   <- i
            result <- getInitialSample(prev.keySet)
          } yield prev + result
        }
      picCollection <- getPointInCubeCollection(
                         logPdfPriorResults.values.toVector,
                         modelParameterCollectionToRawVector
                       )
      _ <- (for {
             _       <- sampleDomain.set(picCollection)
             _       <- samplePool.set(logPdfPriorResults)
             samples <- samplePool.get
             _       <- samplePoolMinimumLogPdf.set(samples.keySet.min)
           } yield ()).commit.toResultM
    } yield ()

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] def buildSampleSimulation: ResultOrErrF[F, Unit] =
    (for {
      _ <- initialise.value
      _ <- samplingRecursion.start
      _ <- domainRebuildRecursion.start
    } yield ()).toResultM

  private[thylacine] def waitForSimulationConstruction: ResultOrErrF[F, Unit] =
    (for {
      samplingSimulationRaw <- samplingSimulation.get
      _                     <- STM[F].waitFor(samplingSimulationRaw.isConstructed)
    } yield ()).commit.toResultM

  // Run assuming the sample simulation has completed. It is possible to
  // put a `waitForSimulationConstruction` in this call but it adds unnecessary
  // overhead on the transaction runtime
  protected def getSimulatedSample: ResultOrErrF[F, ModelParameterCollection[F]] =
    for {
      sampleSimulationRaw <- samplingSimulation.get.commit.toResultM
      result              <- sampleSimulationRaw.getSample
    } yield result

  // Assumes the quadratures have been fully constructed (see above comment).
  // We return the mean for the integrations, as SLQ produces inferences of these
  // integrations.
  protected override final def integrateOverModelParameters(
      integrand: BigDecimal => BigDecimal
  ): ResultOrErrF[F, BigDecimal] =
    for {
      quadratureRaw <- quadratureIntegrations.get.commit.toResultM
      result        <- quadratureRaw.getIntegrationStats(integrand)
    } yield result.sum / result.size

  override protected def sampleModelParameters: ResultOrErrF[F, ModelParameterCollection[F]] =
    getSimulatedSample

  override protected def rawSampleModelParameters: ResultOrErrF[F, VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)

}

private[thylacine] object SlqEngine {

  private[thylacine] def getPointInCubeCollection[F[_]: Async](
      inputs: Vector[ModelParameterCollection[F]],
      toDenseVector: ModelParameterCollection[F] => ResultOrErrF[F, DenseVector[Double]]
  ): ResultOrErrF[F, PointInCubeCollection[F]] = {
    def getPointInCube(
        input: ModelParameterCollection[F],
        toDenseVector: ModelParameterCollection[F] => ResultOrErrF[F, DenseVector[Double]]
    ): ResultOrErrF[F, PointInCube[F]] =
      for {
        rawDenseVector <- toDenseVector(input)
      } yield PointInCube(
        rawDenseVector.toArray.toVector.map(PointInInterval(_)),
        validated = true
      )

    for {
      pics   <- inputs.parTraverse(i => getPointInCube(i, toDenseVector))
      result <- PointInCubeCollection(pics, validated = true).readyForSampling
    } yield result
  }
}
