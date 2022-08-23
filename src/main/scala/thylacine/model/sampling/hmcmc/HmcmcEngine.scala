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
package thylacine.model.sampling.hmcmc

import bengal.stm._
import thylacine.model.components.posterior._
import thylacine.model.components.prior.Prior
import thylacine.model.core.Erratum.{ResultOrErrIo, _}
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.implicits._

import scala.collection.immutable.Queue

/** Implementation of the Hamiltonian MCMC sampling algorithm
  */
private[thylacine] abstract class HmcmcEngine(implicit stm: STM[IO]) extends ModelParameterSampler {

  import HmcmcEngine._
  import stm._

  protected def posterior: Posterior[Prior[_], _]

  /*
   * - - -- --- ----- -------- -------------
   * Configuration
   * - - -- --- ----- -------- -------------
   */

  protected def simulationsBetweenSamples: Int
  protected def stepsInSimulation: Int
  protected def simulationInitialEpsilon: Double
  protected def maxEpsilonHistory: Int
  protected def targetAcceptance: Double
  protected def warmUpSimulationCount: Int
  protected def sampleParallelism: Int

  protected def startingPoint: ResultOrErrIo[ModelParameterCollection]

  protected def sampleRequestUpdateCallback: Int => Unit
  protected def sampleRequestSetCallback: Int => Unit
  protected def dhMonitorCallback: Double => Unit
  protected def epsilonUpdateCallback: Double => Unit

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  private val sampleRequests: TxnVar[Queue[SampleRequest]] =
    TxnVar.of(Queue[SampleRequest]()).unsafeRunSync()

  private val currentMcmcPositions: TxnVar[Queue[ModelParameterCollection]] =
    TxnVar.of(Queue[IndexedVectorCollection]()).unsafeRunSync()

  private val burnInComplete: TxnVar[Boolean] =
    TxnVar.of(false).unsafeRunSync()

  private val simulationEpsilon: TxnVar[Double] =
    TxnVar.of(0.1).unsafeRunSync()

  private val epsilonAdjustmentResults: TxnVar[Queue[Double]] =
    TxnVar.of(Queue[Double]()).unsafeRunSync()

  private val parallelismTokenPool: TxnVar[Int] =
    TxnVar.of(2).unsafeRunSync()

  /*
   * - - -- --- ----- -------- -------------
   * HMCMC
   * - - -- --- ----- -------- -------------
   */

  private def updateSimulationEpsilon(
      success: Boolean,
      iteration: Int
  ): ResultOrErrIo[Unit] = {
    def multiplier(goUp: Boolean) =
      Math.max(1d + (if (goUp) 1.0 else -1.0) * (1d - (Math
                 .min(iteration, warmUpSimulationCount)
                 .toDouble / warmUpSimulationCount)),
               .01
      )

    ResultOrErrIo.fromIo {
      for {
        result <- (for {
                    _ <- if (success) {
                           epsilonAdjustmentResults.modify(
                             _.enqueue(1.0).takeRight(maxEpsilonHistory)
                           )
                         } else {
                           epsilonAdjustmentResults.modify(
                             _.enqueue(0.0).takeRight(maxEpsilonHistory)
                           )
                         }
                    epsilonHistory <- epsilonAdjustmentResults.get
                    newMultiplier =
                      multiplier(
                        epsilonHistory.sum / epsilonHistory.size >= targetAcceptance
                      )
                    _      <- simulationEpsilon.modify(_ * newMultiplier)
                    result <- simulationEpsilon.get
                  } yield result).commit
        _ <- IO(epsilonUpdateCallback(result))
      } yield ()
    }
  }

  private def secureWorkRight: Txn[Unit] =
    for {
      tokenCount <- parallelismTokenPool.get
      _          <- waitFor(tokenCount > 0)
      _          <- parallelismTokenPool.modify(_ - 1)
    } yield ()

  private def getNextPosition: Txn[ModelParameterCollection] =
    for {
      positions <- currentMcmcPositions.get
      _         <- waitFor(positions.nonEmpty)
      (result, newQueue) = positions.dequeue
      _ <- currentMcmcPositions.set(newQueue)
    } yield result

  private def submitNextPosition(
      position: ModelParameterCollection
  ): Txn[Unit] =
    currentMcmcPositions.modify(_.enqueue(position))

  private def runLeapfrogAt(
      input: ModelParameterCollection,
      p: VectorContainer,
      gradLogPdf: ModelParameterCollection,
      iterationCount: Int = 1
  ): ResultOrErrIo[(ModelParameterCollection, VectorContainer)] =
    if (iterationCount > stepsInSimulation) {
      ResultOrErrIo.fromValue((input, p))
    } else {
      (for {
        epsilon <- ResultOrErrIo.fromIo(simulationEpsilon.get.commit)
        p       <- posterior.rawVectorToModelParameterCollection(p.rawVector)
        pNew =
          p.rawSumWith(gradLogPdf.rawScalarMultiplyWith(epsilon / 2))
        xNew = input.rawSumWith(pNew.rawScalarMultiplyWith(epsilon))
        gNew <- posterior.logPdfGradientAt(xNew)
        pNewNew <-
          posterior.modelParameterCollectionToRawVector(
            pNew.rawSumWith(
              gNew.rawScalarMultiplyWith(epsilon / 2)
            )
          )
      } yield (xNew, VectorContainer(pNewNew.toArray.toVector), gNew)).flatMap { i =>
        runLeapfrogAt(i._1, i._2, i._3, iterationCount + 1)
      }
    }

  private def getHamiltonianValue(p: VectorContainer, E: Double): Double =
    p.rawDotProductWith(p) / 2.0 - E

  private def runDynamicSimulationFrom(
      input: ModelParameterCollection,
      maxIterations: Int,
      logPdfOpt: Option[Double] = None,
      gradLogPdfOpt: Option[ModelParameterCollection] = None,
      burnIn: Boolean = false,
      iterationCount: Int = 1
  ): ResultOrErrIo[ModelParameterCollection] =
    if (iterationCount <= maxIterations) {
      (for {
        logPdf <- logPdfOpt match {
                    case Some(res) => ResultOrErrIo.fromValue(res)
                    case _         => posterior.logPdfAt(input)
                  }
        gradLogPdf <- gradLogPdfOpt match {
                        case Some(res) => ResultOrErrIo.fromValue(res)
                        case _         => posterior.logPdfGradientAt(input)
                      }
        p           = VectorContainer.random(posterior.domainDimension)
        hamiltonian = getHamiltonianValue(p, logPdf)
        xAndPNew <- runLeapfrogAt(input, p, gradLogPdf)
        (xNew, pNew) = xAndPNew
        eNew <- posterior.logPdfAt(xNew)
        hNew = getHamiltonianValue(pNew, eNew)
        dH   = hNew - hamiltonian
        _ <- ResultOrErrIo.fromIo(IO(dhMonitorCallback(dH)).start)
        result <- if (dH < 0 || Math.random() < Math.exp(-dH)) {
                    for {
                      _ <- if (burnIn) {
                             updateSimulationEpsilon(success = true, iterationCount)
                           } else {
                             ResultOrErrIo.unit
                           }
                      newGradLogPdf <- posterior.logPdfGradientAt(xNew)
                    } yield (xNew, eNew, newGradLogPdf)
                  } else {
                    for {
                      _ <- if (burnIn) {
                             updateSimulationEpsilon(success = false, iterationCount)
                           } else {
                             ResultOrErrIo.unit
                           }
                    } yield (input, logPdf, gradLogPdf)
                  }
      } yield result).flatMap { r =>
        runDynamicSimulationFrom(r._1, maxIterations, Some(r._2), Some(r._3), burnIn, iterationCount + 1)
      }
    } else {
      ResultOrErrIo.fromValue(input)
    }

  /*
   * - - -- --- ----- -------- -------------
   * Sampling calculation
   * - - -- --- ----- -------- -------------
   */

  private def setAndAcquireNewSample(
      position: ModelParameterCollection
  ): IO[ModelParameterCollection] =
    runDynamicSimulationFrom(position, simulationsBetweenSamples).value.flatMap {
      case Right(res) if res != position =>
        submitNextPosition(res).commit >> IO.pure(res)
      case Right(_) =>
        setAndAcquireNewSample(position)
      case Left(err) =>
        IO.raiseError(new RuntimeException(err.message))
    }

  private def getNextRequest: Txn[SampleRequest] =
    for {
      requests <- sampleRequests.get
      _        <- stm.waitFor(requests.nonEmpty)
      result   <- stm.pure(requests.dequeue)
      _        <- sampleRequests.set(result._2)
    } yield result._1

  private def processRequest: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      secureWorkRight.commit >> (for {
        request   <- getNextRequest.commit
        position  <- getNextPosition.commit
        newSample <- setAndAcquireNewSample(position)
        remaining <- sampleRequests.get.map(_.size).commit
        _         <- IO(sampleRequestUpdateCallback(remaining)).start
        _         <- request.complete(newSample)
        _         <- parallelismTokenPool.modify(_ + 1).commit
      } yield ()).start.void
    }

  private def processingRecursion: ResultOrErrIo[Unit] =
    processRequest.flatMap(_ => processingRecursion)

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private def samplePriors: ResultOrErrIo[ModelParameterCollection] =
    for {
      sampleCollection <-
        posterior.priors.toVector.parTraverse(_.sampleModelParameters)
    } yield sampleCollection.reduce(_ rawMergeWith _)

  private def burnIn: ResultOrErrIo[ModelParameterCollection] =
    for {
      possibleStartingPoint <- startingPoint
      priorSample <-
        if (possibleStartingPoint == IndexedVectorCollection.empty) {
          samplePriors
        } else {
          ResultOrErrIo.fromValue(possibleStartingPoint)
        }
      result <-
        runDynamicSimulationFrom(priorSample, warmUpSimulationCount, burnIn = true)
    } yield result

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] def initialise: IO[Unit] =
    for {
      _             <- burnInComplete.set(false).commit
      _             <- epsilonAdjustmentResults.set(Queue()).commit
      _             <- simulationEpsilon.set(simulationInitialEpsilon).commit
      startPosition <- burnIn.value
      _ <- startPosition match {
             case Right(result) =>
               (for {
                 _ <- parallelismTokenPool.set(sampleParallelism)
                 _ <- currentMcmcPositions.set(
                        Queue.fill(sampleParallelism)(result)
                      )
                 _ <- burnInComplete.set(true)
               } yield ()).commit
             case Left(err) =>
               IO.raiseError(new RuntimeException(err.message))
           }
      _ <- processingRecursion.value.start
    } yield ()

  private[thylacine] def waitForInitialisation: IO[Unit] =
    (for {
      burnInCompleted <- burnInComplete.get
      _               <- stm.waitFor(burnInCompleted)
    } yield ()).commit

  protected def getHmcmcSample: ResultOrErrIo[ModelParameterCollection] =
    ResultOrErrIo.fromIo {
      for {
        deferred <- Deferred[IO, ModelParameterCollection]
        newRequestSize <- (for {
                            _                 <- sampleRequests.modify(_.enqueue(deferred))
                            sampleRequestSize <- sampleRequests.get.map(_.size)
                          } yield sampleRequestSize).commit
        _      <- IO(sampleRequestSetCallback(newRequestSize)).start
        result <- deferred.get
      } yield result
    }

  override protected def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    getHmcmcSample

  override protected def rawSampleModelParameters: ResultOrErrIo[VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- posterior.modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)

}

private[thylacine] object HmcmcEngine {

  private[thylacine] type SampleRequest = Deferred[IO, ModelParameterCollection]
}
