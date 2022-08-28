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
import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.components.posterior._
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{IndexedVectorCollection, VectorContainer}
import thylacine.model.sampling.ModelParameterSampler

import cats.effect.Deferred
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.collection.immutable.Queue

/** Implementation of the Hamiltonian MCMC sampling algorithm
  */
private[thylacine] trait HmcmcEngine[F[_]] extends ModelParameterSampler[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  import HmcmcEngine._

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

  protected def startingPoint: ResultOrErrF[F, ModelParameterCollection[F]]

  protected def sampleRequestUpdateCallback: Int => Unit
  protected def sampleRequestSetCallback: Int => Unit
  protected def dhMonitorCallback: Double => Unit
  protected def epsilonUpdateCallback: Double => Unit

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  protected val sampleRequests: TxnVar[F, Queue[SampleRequest[F]]]

  protected val currentMcmcPositions: TxnVar[F, Queue[ModelParameterCollection[F]]]

  protected val burnInComplete: TxnVar[F, Boolean]

  protected val simulationEpsilon: TxnVar[F, Double]

  protected val epsilonAdjustmentResults: TxnVar[F, Queue[Double]]

  protected val parallelismTokenPool: TxnVar[F, Int]

  /*
   * - - -- --- ----- -------- -------------
   * HMCMC
   * - - -- --- ----- -------- -------------
   */

  private def updateSimulationEpsilon(
      success: Boolean,
      iteration: Int
  ): ResultOrErrF[F, Unit] = {
    def multiplier(goUp: Boolean) =
      Math.max(1d + (if (goUp) 1.0 else -1.0) * (1d - (Math
                 .min(iteration, warmUpSimulationCount)
                 .toDouble / warmUpSimulationCount)),
               .01
      )

    (for {
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
      _ <- Async[F].delay(epsilonUpdateCallback(result))
    } yield ()).toResultM
  }

  private val secureWorkRight: Txn[Unit] =
    for {
      tokenCount <- parallelismTokenPool.get
      _          <- STM[F].waitFor(tokenCount > 0)
      _          <- parallelismTokenPool.modify(_ - 1)
    } yield ()

  private val getNextPosition: Txn[ModelParameterCollection[F]] =
    for {
      positions <- currentMcmcPositions.get
      _         <- STM[F].waitFor(positions.nonEmpty)
      (result, newQueue) = positions.dequeue
      _ <- currentMcmcPositions.set(newQueue)
    } yield result

  private def submitNextPosition(
      position: ModelParameterCollection[F]
  ): Txn[Unit] =
    currentMcmcPositions.modify(_.enqueue(position))

  private def runLeapfrogAt(
      input: ModelParameterCollection[F],
      p: VectorContainer,
      gradLogPdf: ModelParameterCollection[F],
      iterationCount: Int = 1
  ): ResultOrErrF[F, (ModelParameterCollection[F], VectorContainer)] =
    if (iterationCount > stepsInSimulation) {
      (input, p).toResultM
    } else {
      (for {
        epsilon <- simulationEpsilon.get.commit.toResultM
        p       <- rawVectorToModelParameterCollection(p.rawVector)
        pNew =
          p.rawSumWith(gradLogPdf.rawScalarMultiplyWith(epsilon / 2))
        xNew = input.rawSumWith(pNew.rawScalarMultiplyWith(epsilon))
        gNew <- logPdfGradientAt(xNew)
        pNewNew <-
          modelParameterCollectionToRawVector(
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
      input: ModelParameterCollection[F],
      maxIterations: Int,
      logPdfOpt: Option[Double] = None,
      gradLogPdfOpt: Option[ModelParameterCollection[F]] = None,
      burnIn: Boolean = false,
      iterationCount: Int = 1
  ): ResultOrErrF[F, ModelParameterCollection[F]] =
    if (iterationCount <= maxIterations) {
      (for {
        logPdf <- logPdfOpt match {
                    case Some(res) => res.toResultM
                    case _         => logPdfAt(input)
                  }
        gradLogPdf <- gradLogPdfOpt match {
                        case Some(res) => res.toResultM
                        case _         => logPdfGradientAt(input)
                      }
        p           = VectorContainer.random(domainDimension)
        hamiltonian = getHamiltonianValue(p, logPdf)
        xAndPNew <- runLeapfrogAt(input, p, gradLogPdf)
        (xNew, pNew) = xAndPNew
        eNew <- logPdfAt(xNew)
        hNew = getHamiltonianValue(pNew, eNew)
        dH   = hNew - hamiltonian
        _ <- Async[F].delay(dhMonitorCallback(dH)).start.toResultM
        result <- if (dH < 0 || Math.random() < Math.exp(-dH)) {
                    for {
                      _ <- if (burnIn) {
                             updateSimulationEpsilon(success = true, iterationCount)
                           } else {
                             ResultOrErrF.unit
                           }
                      newGradLogPdf <- logPdfGradientAt(xNew)
                    } yield (xNew, eNew, newGradLogPdf)
                  } else {
                    for {
                      _ <- if (burnIn) {
                             updateSimulationEpsilon(success = false, iterationCount)
                           } else {
                             ResultOrErrF.unit
                           }
                    } yield (input, logPdf, gradLogPdf)
                  }
      } yield result).flatMap { r =>
        runDynamicSimulationFrom(r._1, maxIterations, Some(r._2), Some(r._3), burnIn, iterationCount + 1)
      }
    } else {
      input.toResultM
    }

  /*
   * - - -- --- ----- -------- -------------
   * Sampling calculation
   * - - -- --- ----- -------- -------------
   */

  private def setAndAcquireNewSample(
      position: ModelParameterCollection[F]
  ): F[ModelParameterCollection[F]] =
    runDynamicSimulationFrom(position, simulationsBetweenSamples).value.flatMap {
      case Right(res) if res != position =>
        submitNextPosition(res).commit >> Async[F].pure(res)
      case Right(_) =>
        setAndAcquireNewSample(position)
      case Left(err) =>
        Async[F].raiseError(new RuntimeException(err.message))
    }

  private val getNextRequest: Txn[SampleRequest[F]] =
    for {
      requests <- sampleRequests.get
      _        <- STM[F].waitFor(requests.nonEmpty)
      result   <- STM[F].delay(requests.dequeue)
      _        <- sampleRequests.set(result._2)
    } yield result._1

  private val processRequest: ResultOrErrF[F, Unit] =
    (secureWorkRight.commit >> (for {
      request   <- getNextRequest.commit
      position  <- getNextPosition.commit
      newSample <- setAndAcquireNewSample(position)
      remaining <- sampleRequests.get.map(_.size).commit
      _         <- Async[F].delay(sampleRequestUpdateCallback(remaining)).start
      _         <- request.complete(newSample)
      _         <- parallelismTokenPool.modify(_ + 1).commit
    } yield ()).start.void).toResultM

  private val processingRecursion: ResultOrErrF[F, Unit] =
    processRequest.flatMap(_ => processingRecursion)

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private val burnIn: ResultOrErrF[F, ModelParameterCollection[F]] =
    for {
      possibleStartingPoint <- startingPoint
      priorSample <-
        if (possibleStartingPoint == IndexedVectorCollection.empty) {
          samplePriors
        } else {
          possibleStartingPoint.toResultM
        }
      result <-
        runDynamicSimulationFrom(priorSample, warmUpSimulationCount, burnIn = true)
    } yield result

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] val initialise: F[Unit] =
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
               Async[F].raiseError(new RuntimeException(err.message))
           }
      _ <- processingRecursion.value.start
    } yield ()

  private[thylacine] val waitForInitialisation: F[Unit] =
    (for {
      burnInCompleted <- burnInComplete.get
      _               <- STM[F].waitFor(burnInCompleted)
    } yield ()).commit

  protected val getHmcmcSample: ResultOrErrF[F, ModelParameterCollection[F]] =
    (for {
      deferred <- Deferred[F, ModelParameterCollection[F]]
      newRequestSize <- (for {
                          _                 <- sampleRequests.modify(_.enqueue(deferred))
                          sampleRequestSize <- sampleRequests.get.map(_.size)
                        } yield sampleRequestSize).commit
      _      <- Async[F].delay(sampleRequestSetCallback(newRequestSize)).start
      result <- deferred.get
    } yield result).toResultM

  override protected val sampleModelParameters: ResultOrErrF[F, ModelParameterCollection[F]] =
    getHmcmcSample

  override protected val rawSampleModelParameters: ResultOrErrF[F, VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)

}

private[thylacine] object HmcmcEngine {

  private[thylacine] type SampleRequest[F[_]] = Deferred[F, ModelParameterCollection[F]]
}
