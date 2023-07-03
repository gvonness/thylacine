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
import thylacine.model.core.telemetry.HmcmcTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{IndexedVectorCollection, VectorContainer}
import thylacine.model.sampling.ModelParameterSampler

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.collection.immutable.Queue

/** Implementation of the Hamiltonian MCMC sampling algorithm
  */
private[thylacine] trait HmcmcEngine[F[_]] extends ModelParameterSampler[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  /*
   * - - -- --- ----- -------- -------------
   * Configuration
   * - - -- --- ----- -------- -------------
   */
  protected def sampleParallelism: Int
  protected def simulationsBetweenSamples: Int
  protected def stepsInSimulation: Int
  protected def simulationEpsilon: Double
  protected def warmUpSimulationCount: Int

  protected def startingPoint: F[ModelParameterCollection]
  protected def sampleProcessedCallback: HmcmcTelemetryUpdate => F[Unit]

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  protected val currentMcmcPositions: TxnVar[F, Queue[ModelParameterCollection]]

  protected val workTokenPool: TxnVar[F, Int]

  protected val numberOfSamplesRemaining: TxnVar[F, Int]

  protected val burnInComplete: TxnVar[F, Boolean]

  protected val jumpAcceptances: TxnVar[F, Int]

  protected val jumpAttempts: TxnVar[F, Int]

  /*
   * - - -- --- ----- -------- -------------
   * HMCMC
   * - - -- --- ----- -------- -------------
   */

  private val secureWorkRight: Txn[Unit] =
    for {
      numberOfTokens <- workTokenPool.get
      _              <- STM[F].waitFor(numberOfTokens > 0)
      _              <- workTokenPool.modify(_ - 1)
    } yield ()

  private def runLeapfrogAt(
      input: ModelParameterCollection,
      rawP: VectorContainer,
      gradNegLogPdf: ModelParameterCollection,
      iterationCount: Int = 1
  ): F[(ModelParameterCollection, VectorContainer)] =
    if (iterationCount > stepsInSimulation) {
      Async[F].pure((input, rawP))
    } else {
      (for {
        p <- Async[F].delay(rawVectorToModelParameterCollection(rawP.rawVector))
        pNew <-
          Async[F].delay(p.rawSumWith(gradNegLogPdf.rawScalarMultiplyWith(-simulationEpsilon / 2)))
        xNew <- Async[F].delay(input.rawSumWith(pNew.rawScalarMultiplyWith(simulationEpsilon)))
        gNew <- logPdfGradientAt(xNew).map(_.rawScalarMultiplyWith(-1))
        pNewNew <- Async[F].delay {
                     modelParameterCollectionToRawVector(
                       pNew.rawSumWith(
                         gNew.rawScalarMultiplyWith(-simulationEpsilon / 2)
                       )
                     )
                   }
      } yield (xNew, VectorContainer(pNewNew.toArray.toVector), gNew)).flatMap { case (xNew, pNewNew, gNew) =>
        runLeapfrogAt(xNew, pNewNew, gNew, iterationCount + 1)
      }
    }

  private def getHamiltonianValue(p: VectorContainer, E: Double): Double =
    p.rawDotProductWith(p) / 2.0 + E

  private def runDynamicSimulationFrom(
      input: ModelParameterCollection,
      maxIterations: Int,
      logPdfOpt: Option[Double] = None,
      gradLogPdfOpt: Option[ModelParameterCollection] = None,
      burnIn: Boolean = false,
      iterationCount: Int = 1
  ): F[ModelParameterCollection] =
    if (iterationCount <= maxIterations) {
      (for {
        _ <- if (burnIn) {
          Async[F].delay(print(s"\rHMCMC Sampling :: Burn-in Iteration - $iterationCount/$warmUpSimulationCount"))
        } else {
          Async[F].unit
        }
        negLogPdf <- logPdfOpt match {
                       case Some(res) => Async[F].pure(res)
                       case _         => logPdfAt(input).map(_ * -1)
                     }
        gradNegLogPdf <- gradLogPdfOpt match {
                           case Some(res) => Async[F].pure(res)
                           case _         => logPdfGradientAt(input).map(_.rawScalarMultiplyWith(-1))
                         }
        p           <- Async[F].delay(VectorContainer.random(domainDimension))
        hamiltonian <- Async[F].delay(getHamiltonianValue(p, negLogPdf))
        xAndPNew    <- runLeapfrogAt(input, p, gradNegLogPdf)
        (xNew, pNew) = xAndPNew
        eNew <- logPdfAt(xNew).map(_ * -1)
        hNew <- Async[F].delay(getHamiltonianValue(pNew, eNew))
        dH   <- Async[F].delay(hNew - hamiltonian)
        _ <- if (burnIn) {
          Async[F].unit
        } else {
          Async[F].delay(print(s"\rHMCMC Sampling :: exp(-dH) = ${Math.exp(-dH)}"))
        }
        result <- Async[F].ifM(Async[F].delay(dH < 0 || Math.random() < Math.exp(-dH)))(
                    for {
                      _ <- Async[F].ifM(Async[F].pure(burnIn))(
                             Async[F].unit,
                             (jumpAcceptances.modify(_ + 1) >> jumpAttempts.modify(_ + 1)).commit
                           )
                      newGradNegLogPdf <- logPdfGradientAt(xNew).map(_.rawScalarMultiplyWith(-1))
                    } yield (xNew, eNew, newGradNegLogPdf),
                    for {
                      _ <- Async[F].ifM(Async[F].pure(burnIn))(
                             Async[F].unit,
                             jumpAttempts.modify(_ + 1).commit
                           )
                    } yield (input, negLogPdf, gradNegLogPdf)
                  )
      } yield result).flatMap { case (xNew, eNew, gNew) =>
        runDynamicSimulationFrom(xNew, maxIterations, Some(eNew), Some(gNew), burnIn, iterationCount + 1)
      }
    } else {
      Async[F].pure(input)
    }

  /*
   * - - -- --- ----- -------- -------------
   * Sampling calculation
   * - - -- --- ----- -------- -------------
   */

  private def setAndAcquireNewSample(
      position: ModelParameterCollection
  ): F[ModelParameterCollection] =
    runDynamicSimulationFrom(position, simulationsBetweenSamples).flatMap {
      case result if result != position =>
        currentMcmcPositions.modify(_.enqueue(result)).commit >> Async[F].pure(result)
      case _ =>
        setAndAcquireNewSample(position)
    }

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private lazy val burnIn: F[List[ModelParameterCollection]] =
    for {
      possibleStartingPoint <- startingPoint
      priorSample <-
        if (possibleStartingPoint == IndexedVectorCollection.empty) {
          samplePriors
        } else {
          Async[F].pure(possibleStartingPoint)
        }
      results <- List.fill(sampleParallelism)(priorSample).parTraverse { startPt =>
                   runDynamicSimulationFrom(startPt, warmUpSimulationCount, burnIn = true)
                 }
    } yield results

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] lazy val launchInitialisation: F[Unit] =
    for {
      _            <- burnInComplete.set(false).commit
      burninResult <- burnIn
      _ <- (for {
             _ <- workTokenPool.set(sampleParallelism)
             _ <- currentMcmcPositions.set(Queue.from(burninResult))
             _ <- burnInComplete.set(true)
             _ <- numberOfSamplesRemaining.set(0)
             _ <- jumpAttempts.set(0)
             _ <- jumpAcceptances.set(0)
           } yield ()).commit
    } yield ()

  private[thylacine] val waitForInitialisationCompletion: F[Unit] =
    (for {
      burnInCompleted <- burnInComplete.get
      _               <- STM[F].waitFor(burnInCompleted)
    } yield ()).commit

  private[thylacine] def sampleUpdateReport(incrementAmount: Int): F[Unit] =
    for {
      numberOfSamples <- (numberOfSamplesRemaining.modify(_ + incrementAmount) >> numberOfSamplesRemaining.get).commit
      jumpsAndAttempts <- (for {
                            jumps    <- jumpAcceptances.get
                            attempts <- jumpAttempts.get
                          } yield (jumps, attempts)).commit
      telemetryResult <- Async[F].delay(HmcmcTelemetryUpdate(numberOfSamples, jumpsAndAttempts._2, jumpsAndAttempts._1))
      _               <- sampleProcessedCallback(telemetryResult)
    } yield ()

  protected val getHmcmcSample: F[ModelParameterCollection] =
    for {
      _ <- sampleUpdateReport(1).start
      position <- (for {
                    _      <- secureWorkRight
                    result <- currentMcmcPositions.get.map(_.dequeue)
                    _      <- currentMcmcPositions.set(result._2)
                  } yield result._1).commit
      newSample <- setAndAcquireNewSample(position)
      _         <- workTokenPool.modify(_ + 1).commit
      _         <- sampleUpdateReport(-1).start
    } yield newSample

  protected override val sampleModelParameters: F[ModelParameterCollection] =
    getHmcmcSample

  protected override val rawSampleModelParameters: F[VectorContainer] =
    sampleModelParameters.map(s => VectorContainer(modelParameterCollectionToRawVector(s)))

}
