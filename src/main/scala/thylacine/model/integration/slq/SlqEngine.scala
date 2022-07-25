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

import bengal.stm._
import thylacine.model.components.posterior._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._
import thylacine.model.integration.slq.SamplingSimulation._
import thylacine.model.interface.SlqTelemetryUpdate

import breeze.linalg.DenseVector
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

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
private[thylacine] abstract class SlqEngine(implicit stm: STM[IO])
    extends ModelParameterIntegrator
    with ModelParameterSampler {

  import SlqEngine._
  import stm._

  protected def posterior: Posterior[Prior[_], _]

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
  protected def seeds: Set[ModelParameterCollection]

  protected def slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit
  protected def domainRebuildStartCallback: Unit => Unit
  protected def domainRebuildFinishCallback: Unit => Unit

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  private val sampleDomain: TxnVar[PointInCubeCollection] =
    TxnVar.of(PointInCubeCollection.empty).unsafeRunSync()

  //
  private val samplePool: TxnVarMap[Double, ModelParameterCollection] =
    TxnVarMap.of(Map[Double, ModelParameterCollection]()).unsafeRunSync()

  private val samplePoolMinimumLogPdf: TxnVar[Double] =
    TxnVar.of(-Double.MaxValue).unsafeRunSync()

  //
  private val logPdfResults: TxnVar[Vector[(Double, ModelParameterCollection)]] =
    TxnVar.of(Vector[(Double, ModelParameterCollection)]()).unsafeRunSync()

  //
  private val sampleDomainScalingState: TxnVar[QuadratureDomainTelemetry] =
    TxnVar.of(QuadratureDomainTelemetry.init).unsafeRunSync()

  private val workTokenPool: TxnVar[Int] =
    TxnVar.of(0).unsafeRunSync()

  //
  private val abscissas: TxnVar[QuadratureAbscissaCollection] =
    TxnVar.of(QuadratureAbscissaCollection.init).unsafeRunSync()

  //
  private val quadratureIntegrations: TxnVar[QuadratureIntegrator] =
    TxnVar.of(QuadratureIntegrator.empty).unsafeRunSync()

  private val samplingSimulation: TxnVar[SamplingSimulation] =
    TxnVar.of(SamplingSimulation.empty).unsafeRunSync()

  private val isConverged: TxnVar[Boolean] =
    TxnVar.of(false).unsafeRunSync()

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
        case _       => stm.pure(logPdf)
      }
      .handleErrorWith(_ => stm.pure(logPdf))
  }

  private def recordMinimumLogPdf(
      minLogPdf: Option[Double] = None
  ): Txn[Unit] =
    for {
      currentMinimum <- minLogPdf match {
                          case Some(res) =>
                            stm.pure(res)
                          case _ => samplePoolMinimumLogPdf.get
                        }
      currentMinValue <- samplePool.get(currentMinimum)
      _               <- stm.waitFor(currentMinValue.isDefined)
      _ <- currentMinValue match {
             case Some(value) =>
               for {
                 _ <- logPdfResults.modify {
                        (currentMinimum, value) +: _
                      }
                 _       <- samplePool.remove(currentMinimum)
                 samples <- samplePool.get
                 newMinimum <- if (samples.nonEmpty) {
                                 stm.pure(samples.keys.min)
                               } else {
                                 stm.pure(Double.MinValue)
                               }
                 _ <- samplePoolMinimumLogPdf.set(newMinimum)
               } yield ()
             case _ =>
               stm.abort(
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
           } else stm.unit
    } yield ()

  private def drainSamplePool: IO[Unit] =
    (for {
      samplePoolNonEmpty <- samplePool.get.map(_.nonEmpty)
      result <- if (samplePoolNonEmpty) {
                  recordMinimumLogPdf() >>
                    stm.pure(true)
                } else {
                  stm.pure(false)
                }
    } yield result).commit.flatMap(continue => if (continue) drainSamplePool else IO.unit)

  private def updateStateWithSampleCalculation(
      logPdf: Double,
      modelParameters: ModelParameterCollection
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
                       sampleDomainScalingState.modify(_.addRejection) >> stm
                         .pure(false)
                     }
    } yield outerResult

  private lazy val sampleAndAnalyze: ResultOrErrIo[Unit] =
    for {
      domain <- ResultOrErrIo.fromIo(sampleDomain.get.commit)
      scalingFactorTelemetry <-
        ResultOrErrIo.fromIo(sampleDomainScalingState.get.commit)
      sampleContainer <-
        domain.getSample(scalingFactorTelemetry.currentScaleFactor)
      sample <-
        posterior.rawVectorToModelParameterCollection(sampleContainer.rawVector)
      logPdf <- posterior.logPdfAt(sample)
      result <-
        ResultOrErrIo.fromIo(
          updateStateWithSampleCalculation(logPdf, sample).commit
        )
      _ <-
        if (
          result || (scalingFactorTelemetry.rejectionStreak > 0 && scalingFactorTelemetry.rejectionStreak % 100 == 0)
        ) {
          for {
            _ <- if (result) setConverged() else ResultOrErrIo.unit
            telemetry <- ResultOrErrIo.fromIo {
                           (for {
                             samples      <- samplePool.get
                             integrations <- quadratureIntegrations.get
                           } yield (samples, scalingFactorTelemetry, integrations)).commit
                         }
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
          ResultOrErrIo.unit
        }
    } yield ()

  private def sampleAndReplaceToken: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromResultOrErrorIo {
      for {
        result <- sampleAndAnalyze.value
        _      <- workTokenPool.modify(_ + 1).commit
      } yield result
    }

  private def getWorkToken: Txn[Unit] =
    for {
      tokenAmount <- workTokenPool.get
      _           <- stm.waitFor(tokenAmount > 0)
      _           <- workTokenPool.set(tokenAmount - 1)
    } yield ()

  private def setConverged(): ResultOrErrIo[Unit] = {
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
      telemetry <- ResultOrErrIo.fromIo {
                     (for {
                       quadratures    <- quadratureIntegrations.get
                       iterationCount <- logPdfResults.get.map(_.size)
                       domainExhausted <-
                         sampleDomainScalingState.get.map(_.isConverged)
                     } yield (quadratures, iterationCount, domainExhausted)).commit
                   }
      negEntStats <- telemetry._1.negativeEntropyStats
      _ <- ResultOrErrIo.fromIo {
             isConverged
               .set(
                 testConverged(telemetry._2, slqSamplePoolSize, negEntStats) || telemetry._3
               )
               .commit
           }
    } yield ()
  }

  private def initiateSampling: IO[Boolean] =
    for {
      _                 <- getWorkToken.commit
      _                 <- sampleAndReplaceToken.value.start
      continueIteration <- isConverged.get.map(!_).commit
    } yield continueIteration

  private def samplingRecursion: IO[Unit] =
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

  private def getSamplesForDomainRebuild: ResultOrErrIo[Boolean] =
    ResultOrErrIo.fromIo((for {
      samplingDomainState <- sampleDomainScalingState.get
      converged           <- isConverged.get
      _                   <- stm.waitFor(samplingDomainState.initiateRebuild || converged)
    } yield converged).commit)

  private def rebuildDomain: ResultOrErrIo[Boolean] =
    for {
      converged <- getSamplesForDomainRebuild
      continueIteration <- if (!converged) {
                             for {
                               _ <- ResultOrErrIo.fromCalculation(
                                      domainRebuildStartCallback
                                    )
                               samples <-
                                 ResultOrErrIo.fromIo(samplePool.get.commit)
                               picCollection <-
                                 getPointInCubeCollection(
                                   samples.values.toVector,
                                   posterior.modelParameterCollectionToRawVector
                                 )
                               _ <- ResultOrErrIo.fromIo {
                                      (for {
                                        _ <- sampleDomain.set(
                                               picCollection
                                             )
                                        _ <-
                                          sampleDomainScalingState
                                            .modify(
                                              _.resetForRebuild
                                            )
                                      } yield ()).commit
                                    }
                               _ <- setConverged()
                               continueIteration <-
                                 ResultOrErrIo.fromIo(
                                   isConverged.get.map(!_).commit
                                 )
                               _ <- ResultOrErrIo.fromCalculation(
                                      domainRebuildFinishCallback
                                    )
                             } yield continueIteration
                           } else {
                             ResultOrErrIo.fromValue(false)
                           }
    } yield continueIteration

  private def domainRebuildRecursion: IO[Unit] =
    rebuildDomain.value.flatMap {
      case Right(false) => IO.unit
      case _            => domainRebuildRecursion
    }

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private lazy val initialiseTvars: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
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
      } yield ()).commit
    }

  private def analyseSeeds: ResultOrErrIo[Map[Double, ModelParameterCollection]] =
    for {
      result <- seeds.toVector.parTraverse { s =>
                  posterior.logPdfAt(s).map(i => (i, s))
                }
    } yield result.toMap

  private def getInitialSample(
      logPdfs: Set[Double]
  ): ResultOrErrIo[(Double, ModelParameterCollection)] =
    (for {
      sample <- posterior.samplePriors
      logPdf <- posterior.logPdfAt(sample)
    } yield (logPdf, sample)).flatMap { i =>
      if (logPdfs.contains(i._1)) {
        getInitialSample(logPdfs)
      } else {
        ResultOrErrIo.fromValue(i)
      }
    }

  private def initialise: ResultOrErrIo[Unit] =
    for {
      _     <- initialiseTvars
      seeds <- analyseSeeds
      logPdfPriorResults <-
        (1 to Math.max(1, slqSamplePoolSize - seeds.size)).foldLeft(
          ResultOrErrIo.fromValue(seeds)
        ) { (i, _) =>
          for {
            prev   <- i
            result <- getInitialSample(prev.keySet)
          } yield prev + result
        }
      picCollection <- getPointInCubeCollection(
                         logPdfPriorResults.values.toVector,
                         posterior.modelParameterCollectionToRawVector
                       )
      _ <- ResultOrErrIo.fromIo {
             (for {
               _       <- sampleDomain.set(picCollection)
               _       <- samplePool.set(logPdfPriorResults)
               samples <- samplePool.get
               _       <- samplePoolMinimumLogPdf.set(samples.keySet.min)
             } yield ()).commit
           }
    } yield ()

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] def buildSampleSimulation: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- initialise.value
        _ <- samplingRecursion.start
        _ <- domainRebuildRecursion.start
      } yield ()
    }

  private[thylacine] def waitForSimulationConstruction: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      (for {
        samplingSimulationRaw <- samplingSimulation.get
        _                     <- stm.waitFor(samplingSimulationRaw.isConstructed)
      } yield ()).commit
    }

  // Run assuming the sample simulation has completed. It is possible to
  // put a `waitForSimulationConstruction` in this call but it adds unnecessary
  // overhead on the transaction runtime
  protected def getSimulatedSample: ResultOrErrIo[ModelParameterCollection] =
    for {
      sampleSimulationRaw <-
        ResultOrErrIo.fromIo(samplingSimulation.get.commit)
      result <- sampleSimulationRaw.getSample
    } yield result

  // Assumes the quadratures have been fully constructed (see above comment).
  // We return the mean for the integrations, as SLQ produces inferences of these
  // integrations.
  protected override final def integrateOverModelParameters(
      integrand: BigDecimal => BigDecimal
  ): ResultOrErrIo[BigDecimal] =
    for {
      quadratureRaw <- ResultOrErrIo.fromIo(quadratureIntegrations.get.commit)
      result        <- quadratureRaw.getIntegrationStats(integrand)
    } yield result.sum / result.size

  override protected def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    getSimulatedSample

  override protected def rawSampleModelParameters: ResultOrErrIo[VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- posterior.modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)

}

private[thylacine] object SlqEngine {

  private[thylacine] def getPointInCubeCollection(
      inputs: Vector[ModelParameterCollection],
      toDenseVector: ModelParameterCollection => ResultOrErrIo[
        DenseVector[Double]
      ]
  ): ResultOrErrIo[PointInCubeCollection] = {
    def getPointInCube(
        input: ModelParameterCollection,
        toDenseVector: ModelParameterCollection => ResultOrErrIo[
          DenseVector[Double]
        ]
    ): ResultOrErrIo[PointInCube] =
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
