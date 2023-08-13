/*
 * Copyright 2023 Greg von Nessi
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
package thylacine.model.sampling.leapfrog

import bengal.stm.*
import bengal.stm.model.*
import bengal.stm.syntax.all.*
import thylacine.model.components.posterior.*
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.sampling.{ ModelParameterSampler, SampleRequest }
import thylacine.util.MathOps
import thylacine.util.ScalaVectorOps.Implicits.*

import cats.effect.Deferred
import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.syntax.all.*

import scala.collection.immutable.Queue
import scala.util.Random

/** Leapfrog MCMC algorithm.
  */
private[thylacine] trait LeapfrogMcmcEngine[F[_]] extends ModelParameterSampler[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  /*
   * - - -- --- ----- -------- -------------
   * Configuration
   * - - -- --- ----- -------- -------------
   */

  protected def stepsBetweenSamples: Int
  protected def samplePoolSize: Int
  protected def warmUpSimulationCount: Int

  // Don't assume this is symmetric but is positive semi-definite
  protected def distanceCalculation: (Vector[Double], Vector[Double]) => Double

  protected def startingPoint: F[ModelParameterCollection]

  protected def sampleRequestUpdateCallback: Int => F[Unit]

  protected def sampleRequestSetCallback: Int => F[Unit]

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  protected def interSampleDistanceCalculationResults: TxnVar[F, Vector[Vector[Double]]]

  protected def sampleLogPdfs: TxnVar[F, Vector[Double]]

  protected def samplePool: TxnVar[F, Vector[Vector[Double]]]

  protected def currentChainTallies: TxnVar[F, Vector[Int]]

  protected def sampleRequests: TxnVar[F, Queue[SampleRequest[F]]]

  protected def burnInComplete: TxnVar[F, Boolean]

  /*
   * - - -- --- ----- -------- -------------
   * MCMC
   * - - -- --- ----- -------- -------------
   */

  private def getLeapfrogVector(leapFrom: Vector[Double], leapOver: Vector[Double]): Vector[Double] =
    leapOver.scalarMultiplyWith(2.0).subtract(leapFrom)

  private def getDistanceCalculations(
    samplePoints: Vector[Vector[Double]],
    newPoint: Vector[Double],
    replaceIndex: Int,
    jumpIndex: Int
  ): F[(Vector[Double], Double)] =
    for {
      newDistances <- Async[F].delay {
                        samplePoints.zipWithIndex.map {
                          case (vectorValue, index) if index != replaceIndex =>
                            distanceCalculation(newPoint, vectorValue)
                          case _ =>
                            0d
                        }
                      }
      staircase <- Async[F].delay(MathOps.vectorCdfStaircase(newDistances))
      qOfReverseJump <- Async[F].delay {
                          val (lower, upper) = staircase(jumpIndex)
                          upper - lower
                        }
    } yield (newDistances, qOfReverseJump)

  // No point in trying to wrap this in a transaction to enable higher concurrency,
  // as all transactional variables would need to be locked for each proposal
  // generation. I.e. this would still reduce down to synchronous computation.
  private val generateProposal: F[(Boolean, Int)] =
    for {
      sampleIndex           <- Async[F].delay(Random.nextInt(samplePoolSize))
      allDistances          <- interSampleDistanceCalculationResults.get.commit
      distanceCalculations  <- Async[F].delay(allDistances(sampleIndex))
      samples               <- samplePool.get.commit
      staircase             <- Async[F].delay(MathOps.vectorCdfStaircase(distanceCalculations))
      randomContinuousIndex <- Async[F].delay(Random.nextDouble())
      randomDiscreteIndex <-
        Async[F].delay(staircase.indexWhere(sc => sc._1 <= randomContinuousIndex && sc._2 > randomContinuousIndex))
      qOfForwardJump <- Async[F].delay {
                          val (lower, upper) = staircase(randomDiscreteIndex)
                          upper - lower
                        }
      newPoint             <- Async[F].delay(getLeapfrogVector(samples(sampleIndex), samples(randomDiscreteIndex)))
      distancesAndReverseQ <- getDistanceCalculations(samples, newPoint, sampleIndex, randomDiscreteIndex)
      (newDistances, qOfReverseJump) = distancesAndReverseQ
      newLogPdf       <- logPdfAt(vectorValuesToModelParameterCollection(newPoint))
      previousLogPdfs <- sampleLogPdfs.get.commit
      a <- Async[F].delay {
             Math.exp(newLogPdf - previousLogPdfs(sampleIndex)) * qOfReverseJump / qOfForwardJump
           }
      _ <- if (a >= 1 || Math.random() < a) {
             for {
               updatedDistances <- Async[F].delay {
                                     allDistances.zipWithIndex.map { case (oldDistances, index) =>
                                       oldDistances.updated(sampleIndex, distanceCalculation(samples(index), newPoint))
                                     }
                                   }
               _ <- interSampleDistanceCalculationResults.set {
                      updatedDistances.updated(sampleIndex, newDistances)
                    }.commit
               _ <- sampleLogPdfs.set {
                      previousLogPdfs.updated(sampleIndex, newLogPdf)
                    }.commit
               _ <- samplePool.set {
                      samples.updated(sampleIndex, newPoint)
                    }.commit
             } yield ()
           } else {
             Async[F].unit
           }
      tallies      <- currentChainTallies.get.commit
      updatedTally <- Async[F].delay(tallies(sampleIndex) + 1)
      newTallies   <- Async[F].delay(tallies.updated(sampleIndex, updatedTally))
      _            <- currentChainTallies.set(newTallies).commit
    } yield (updatedTally >= stepsBetweenSamples, sampleIndex)

  private val samplingRecursion: F[Int] =
    generateProposal.flatMap {
      case (sampleReady, lastUpdatedIndex) if sampleReady =>
        Async[F].pure(lastUpdatedIndex)
      case _ =>
        samplingRecursion
    }

  private val burnInRecursion: F[Unit] =
    generateProposal.flatMap { _ =>
      for {
        tallies <- currentChainTallies.get.commit
        _ <- if (tallies.forall(_ >= warmUpSimulationCount)) {
               Async[F].unit
             } else {
               burnInRecursion
             }
      } yield ()
    }

  /*
   * - - -- --- ----- -------- -------------
   * Sampling calculation
   * - - -- --- ----- -------- -------------
   */

  private val waitForNextRequest: Txn[Unit] =
    for {
      requests <- sampleRequests.get
      _        <- STM[F].waitFor(requests.nonEmpty)
    } yield ()

  private val getNextRequest: Txn[SampleRequest[F]] =
    for {
      requests <- sampleRequests.get
      result   <- STM[F].delay(requests.dequeue)
      _        <- sampleRequests.set(result._2)
    } yield result._1

  private val processRequest: F[Unit] =
    for {
      _           <- waitForNextRequest.commit
      request     <- getNextRequest.commit
      sampleIndex <- samplingRecursion
      newSample   <- samplePool.get.map(pool => vectorValuesToModelParameterCollection(pool(sampleIndex))).commit
      _           <- currentChainTallies.modify(_.updated(sampleIndex, 0)).commit
      remaining   <- sampleRequests.get.map(_.size).commit
      _           <- sampleRequestUpdateCallback(remaining).start
      _           <- request.complete(newSample)
    } yield ()

  private val processingRecursion: F[Unit] =
    processRequest.flatMap(_ => processingRecursion)

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private def initialisePool(samples: Vector[Vector[Double]]): F[Unit] =
    for {
      distances <- Async[F].delay {
                     samples.map { sample =>
                       samples.map(distanceCalculation(sample, _))
                     }
                   }
      evaluations <- samples.toList.traverse(s => logPdfAt(vectorValuesToModelParameterCollection(s))).map(_.toVector)
      _           <- interSampleDistanceCalculationResults.set(distances).commit
      _           <- sampleLogPdfs.set(evaluations).commit
      _           <- currentChainTallies.set(Vector.fill(samplePoolSize)(0)).commit
    } yield ()

  private lazy val populateSamples: F[Unit] =
    for {
      initialSampleCollection <- (1 to samplePoolSize).toList
                                   .traverse(_ => samplePriors.map(modelParameterCollectionToVectorValues))
                                   .map(_.toVector)
      possibleStartingPoint <- startingPoint
      sampleCollection <- if (possibleStartingPoint == IndexedVectorCollection.empty) {
                            Async[F].pure(initialSampleCollection)
                          } else {
                            Async[F].delay {
                              initialSampleCollection
                                .updated(0, modelParameterCollectionToVectorValues(possibleStartingPoint))
                            }
                          }
      _ <- samplePool.set(sampleCollection).commit
      _ <- initialisePool(sampleCollection)
    } yield ()

  private lazy val burnIn: F[Unit] =
    for {
      _ <- populateSamples
      _ <- burnInRecursion
      _ <- currentChainTallies.set(Vector.fill(samplePoolSize)(0)).commit
      _ <- burnInComplete.set(true).commit
    } yield ()

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] lazy val launchInitialisation: F[Unit] =
    for {
      _ <- burnInComplete.set(false).commit
      _ <- burnIn
      _ <- processingRecursion.start
    } yield ()

  private[thylacine] val waitForInitialisationCompletion: F[Unit] =
    (for {
      burnInCompleted <- burnInComplete.get
      _               <- STM[F].waitFor(burnInCompleted)
    } yield ()).commit

  private val getLeapfrogMcmcSample: F[ModelParameterCollection] =
    for {
      deferred <- Deferred[F, ModelParameterCollection]
      newRequestSize <- (for {
                          _                 <- sampleRequests.modify(_.enqueue(deferred))
                          sampleRequestSize <- sampleRequests.get.map(_.size)
                        } yield sampleRequestSize).commit
      _      <- sampleRequestSetCallback(newRequestSize).start
      result <- deferred.get
    } yield result

  override protected def sampleModelParameters(numberOfSamples: Int): F[Set[ModelParameterCollection]] =
    (1 to numberOfSamples).toList.traverse(_ => getLeapfrogMcmcSample).map(_.toSet)

}
