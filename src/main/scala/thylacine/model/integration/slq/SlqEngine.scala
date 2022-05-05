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
import thylacine.model.components.likelihood._
import thylacine.model.components.posterior._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._
import thylacine.model.integration.slq.SamplingSimulation._
import thylacine.model.support.SlqTelemetryUpdate

import breeze.linalg.DenseVector
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

/** Implementation of the SLQ (pronounced like "slick") algorithm
  * introduced in Appendix B of [3].
  *
  * This is heavy artillery when handling high dimensional inferences
  * (i.e. hundreds of dimensions). In this regime, all approaches have
  * draw backs. This technique derives from Skilling's Nested Sampling
  * technique (see chapter 9 in [1] for an introduction or the Appendix
  * in [2] for a more mathematical analysis of the approach). The primary
  * draw-back with this technique is that it focuses on parts of the
  * solution space where density is known to be highest. However, it may
  * miss areas of isolated density spikes. This can be alleviated to some
  * extent with seeding that includes local maxima (found via global
  * optimisers).
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
trait SlqEngine[T <: Prior[_], U <: Likelihood[_, _]]
    extends ModelParameterSampleGenerator {
  this: Posterior[T, U] =>

  private val stm = STM.runtime[IO].unsafeRunSync()
  import SlqEngine._
  import stm._

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
  private val logPdfResults: TxnVar[List[(Double, ModelParameterCollection)]] =
    TxnVar.of(List[(Double, ModelParameterCollection)]()).unsafeRunSync()

  //
  private val sampleDomainScalingState: TxnVar[SampleDomainTelemetry] =
    TxnVar.of(SampleDomainTelemetry.init).unsafeRunSync()

  private val workTokenPool: TxnVar[Int] =
    TxnVar.of(0).unsafeRunSync()

  //
  private val abscissas: TxnVar[SamplingAbscissaCollection] =
    TxnVar.of(SamplingAbscissaCollection.init).unsafeRunSync()

  //
  private val samplingTelemetry: TxnVar[SamplingTelemetry] =
    TxnVar.of(SamplingTelemetry.empty).unsafeRunSync()

  private val samplingSimulation: TxnVar[SamplingSimulation] =
    TxnVar.of(SamplingSimulation.empty).unsafeRunSync()

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
                        (currentMinimum, value) :: _
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
                 samplingTelemetry.set(
                   SamplingTelemetry(logPdfs, quadratures)
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
    } yield result).commit.flatMap(continue =>
      if (continue) drainSamplePool else IO.unit
    )

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
      sample <- rawVectorToModelParameterCollection(sampleContainer.rawVector)
      logPdf <- logPdfAt(sample)
      result <-
        ResultOrErrIo.fromIo(
          updateStateWithSampleCalculation(logPdf, sample).commit
        )
      _ <- if (result) {
             ResultOrErrIo.fromIo {
               (for {
                 samples   <- samplePool.get
                 telemetry <- samplingTelemetry.get
                 negEnt =
                   telemetry.negativeEntropyStats.sum / telemetry.negativeEntropyStats.size
                 iterationCount = telemetry.logPdfs.size
               } yield (samples,
                        scalingFactorTelemetry,
                        negEnt,
                        iterationCount
               )).commit.map { r =>
                 System.out.print(s"""
                      |----------
                      |System time: ${System.nanoTime}
                      |NegEnt Average: ${r._3}
                      |Current LogPDF: $logPdf
                      |Sample Pool Minimum LogPDF: ${r._1.keySet.min}
                      |Domain volume scaling: ${r._2.currentScaleFactor}
                      |Acceptances since domain rebuild: ${r._2.acceptancesSinceLastRebuild}
                      |Sample pool size: ${r._1.size}
                      |Domain cube count: ${domain.pointsInCube.size}
                      |Iteration count: ${r._4}
                      |""".stripMargin)
               }
             }
           } else if (
             scalingFactorTelemetry.rejectionStreak > 0 && scalingFactorTelemetry.rejectionStreak % 100 == 0
           ) {
             ResultOrErrIo.fromCalculation {
               System.out.print(s"""
                    |----------
                    |System time: ${System.nanoTime}
                    |Domain volume scaling: ${scalingFactorTelemetry.currentScaleFactor}
                    |Acceptances since domain rebuild: ${scalingFactorTelemetry.acceptancesSinceLastRebuild}
                    |Domain cube count: ${domain.pointsInCube.size}
                    |Rejection streak count: ${scalingFactorTelemetry.rejectionStreak}
                    |""".stripMargin)
             }
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

  private def getConverged: Txn[Boolean] =
    for {
      telemetry       <- samplingTelemetry.get
      iterationCount  <- logPdfResults.get.map(_.size)
      domainExhausted <- sampleDomainScalingState.get.map(_.isConverged)
    } yield telemetry.isConverged(iterationCount,
                                  slqSamplePoolSize
    ) || domainExhausted

  private def initiateSampling: IO[Boolean] =
    for {
      _                 <- getWorkToken.commit
      _                 <- sampleAndReplaceToken.value.start
      continueIteration <- getConverged.commit.map(!_)
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
      converged           <- getConverged
      _                   <- stm.waitFor(samplingDomainState.initiateRebuild || converged)
    } yield converged).commit)

  private def rebuildDomain: ResultOrErrIo[Boolean] =
    for {
      converged <- getSamplesForDomainRebuild
      continueIteration <- if (!converged) {
                             for {
                               _ <-
                                 ResultOrErrIo.fromIo(
                                   IO(
                                     println(
                                       s"!!!!!!! Initiating domain rebuild"
                                     )
                                   )
                                 )
                               samples <-
                                 ResultOrErrIo.fromIo(samplePool.get.commit)
                               picCollection <-
                                 getPointInCubeCollection(
                                   samples.values.toList,
                                   modelParameterCollectionToRawVector
                                 )
                               continueIteration <- ResultOrErrIo.fromIo {
                                                      (for {
                                                        _ <- sampleDomain.set(
                                                               picCollection
                                                             )
                                                        _ <-
                                                          sampleDomainScalingState
                                                            .modify(
                                                              _.resetForRebuild
                                                            )
                                                        innerResult <-
                                                          getConverged
                                                      } yield !innerResult).commit
                                                    }
                               _ <- ResultOrErrIo.fromIo(
                                      IO(
                                        println(
                                          s"!!!!!!! Domain rebuild complete"
                                        )
                                      )
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

  private def samplePriors: ResultOrErrIo[ModelParameterCollection] =
    for {
      sampleCollection <- priors.toList.parTraverse(_.sampleModelParameters)
      result <-
        ResultOrErrIo.fromCalculation(sampleCollection.reduce(_ rawMergeWith _))
    } yield result

  private lazy val initialiseTvars: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      (for {
        _ <- sampleDomainScalingState.set {
               SampleDomainTelemetry(
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
            SamplingAbscissaCollection(slqNumberOfAbscissa, slqSamplePoolSize)
          )
        _ <- logPdfResults.set(List())
        _ <- samplingTelemetry.set(SamplingTelemetry.empty)
        _ <- samplingSimulation.set(SamplingSimulation.empty)
      } yield ()).commit
    }

  private def analyseSeeds
      : ResultOrErrIo[Map[Double, ModelParameterCollection]] =
    for {
      result <- seeds.toList.parTraverse { s =>
                  logPdfAt(s).map(i => (i, s))
                }
    } yield result.toMap

  private def getInitialSample(
      logPdfs: Set[Double]
  ): ResultOrErrIo[(Double, ModelParameterCollection)] =
    (for {
      sample <- samplePriors
      logPdf <- logPdfAt(sample)
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
                         logPdfPriorResults.values.toList,
                         modelParameterCollectionToRawVector
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
   * Public Interfaces
   * - - -- --- ----- -------- -------------
   */

  def buildSampleSimulation: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        _ <- initialise.value
        _ <- samplingRecursion.start
        _ <- domainRebuildRecursion.start
      } yield ()
    }

  def waitForSimulationConstruction: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      (for {
        samplingSimulationRaw <- samplingSimulation.get
        _                     <- stm.waitFor(samplingSimulationRaw.isConstructed)
      } yield ()).commit
    }

  protected def getSimulatedSample: ResultOrErrIo[ModelParameterCollection] =
    for {
      sampleSimulationRaw <-
        ResultOrErrIo.fromIo(samplingSimulation.get.commit)
      result <- sampleSimulationRaw.getSample
    } yield result

}

object SlqEngine {

  def getPointInCubeCollection(
      inputs: List[ModelParameterCollection],
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
        rawDenseVector.toScalaVector().toList.map(PointInInterval(_)),
        validated = true
      )

    for {
      pics   <- inputs.parTraverse(i => getPointInCube(i, toDenseVector))
      result <- PointInCubeCollection(pics, validated = true).readyForSampling
    } yield result
  }
}
