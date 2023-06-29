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

  protected def simulationsBetweenSamples: Int
  protected def stepsInSimulation: Int
  protected def simulationInitialEpsilon: Double
  protected def maxEpsilonHistory: Int
  protected def targetAcceptance: Double
  protected def warmUpSimulationCount: Int

  protected def startingPoint: F[ModelParameterCollection]

  protected def dhMonitorCallback: Double => F[Unit]
  protected def epsilonUpdateCallback: Double => F[Unit]

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  protected val currentMcmcPosition: TxnVar[F, Option[ModelParameterCollection]]

  protected val currentlySampling: TxnVar[F, Boolean]

  protected val burnInComplete: TxnVar[F, Boolean]

  protected val simulationEpsilon: TxnVar[F, Double]

  protected val epsilonAdjustmentResults: TxnVar[F, Queue[Double]]

  /*
   * - - -- --- ----- -------- -------------
   * HMCMC
   * - - -- --- ----- -------- -------------
   */

  private def updateSimulationEpsilon(
      success: Boolean,
      iteration: Int
  ): F[Unit] = {
    def multiplier(goUp: Boolean): Double =
      Math.max(1d + (if (goUp) 1.0 else -1.0) * (1d - (Math
                 .min(iteration, warmUpSimulationCount)
                 .toDouble / warmUpSimulationCount)),
               .01
      )

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
                  newMultiplier <- STM[F].delay {
                                     multiplier(
                                       epsilonHistory.sum / epsilonHistory.size >= targetAcceptance
                                     )
                                   }
                  _      <- simulationEpsilon.modify(_ * newMultiplier)
                  result <- simulationEpsilon.get
                } yield result).commit
      _ <- epsilonUpdateCallback(result)
    } yield ()
  }

  private val secureWorkRight: Txn[Unit] =
    for {
      isSampling <- currentlySampling.get
      _          <- STM[F].waitFor(!isSampling)
      _          <- currentlySampling.set(true)
    } yield ()

  private def runLeapfrogAt(
      input: ModelParameterCollection,
      rawP: VectorContainer,
      gradLogPdf: ModelParameterCollection,
      iterationCount: Int = 1
  ): F[(ModelParameterCollection, VectorContainer)] =
    if (iterationCount > stepsInSimulation) {
      Async[F].pure((input, rawP))
    } else {
      (for {
        epsilon <- simulationEpsilon.get.commit
        p       <- Async[F].delay(rawVectorToModelParameterCollection(rawP.rawVector))
        pNew <-
          Async[F].delay(p.rawSumWith(gradLogPdf.rawScalarMultiplyWith(epsilon / 2)))
        xNew <- Async[F].delay(input.rawSumWith(pNew.rawScalarMultiplyWith(epsilon)))
        gNew <- logPdfGradientAt(xNew)
        pNewNew <- Async[F].delay {
                     modelParameterCollectionToRawVector(
                       pNew.rawSumWith(
                         gNew.rawScalarMultiplyWith(epsilon / 2)
                       )
                     )
                   }
      } yield (xNew, VectorContainer(pNewNew.toArray.toVector), gNew)).flatMap { case (xNew, pNewNew, gNew) =>
        runLeapfrogAt(xNew, pNewNew, gNew, iterationCount + 1)
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
  ): F[ModelParameterCollection] =
    if (iterationCount <= maxIterations) {
      (for {
        logPdf <- logPdfOpt match {
                    case Some(res) => Async[F].pure(res)
                    case _         => logPdfAt(input)
                  }
        gradLogPdf <- gradLogPdfOpt match {
                        case Some(res) => Async[F].pure(res)
                        case _         => logPdfGradientAt(input)
                      }
        p           <- Async[F].delay(VectorContainer.random(domainDimension))
        hamiltonian <- Async[F].delay(getHamiltonianValue(p, logPdf))
        xAndPNew    <- runLeapfrogAt(input, p, gradLogPdf)
        (xNew, pNew) = xAndPNew
        eNew <- logPdfAt(xNew)
        hNew <- Async[F].delay(getHamiltonianValue(pNew, eNew))
        dH   <- Async[F].delay(hNew - hamiltonian)
        _    <- dhMonitorCallback(dH).start
        result <- if (dH < 0 || Math.random() < Math.exp(-dH)) {
                    for {
                      _ <- if (burnIn) {
                             updateSimulationEpsilon(success = true, iterationCount)
                           } else {
                             Async[F].unit
                           }
                      newGradLogPdf <- logPdfGradientAt(xNew)
                    } yield (xNew, eNew, newGradLogPdf)
                  } else {
                    for {
                      _ <- if (burnIn) {
                             updateSimulationEpsilon(success = false, iterationCount)
                           } else {
                             Async[F].unit
                           }
                    } yield (input, logPdf, gradLogPdf)
                  }
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
        currentMcmcPosition.set(Option(result)).commit >> Async[F].pure(result)
      case _ =>
        setAndAcquireNewSample(position)
    }

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private lazy val burnIn: F[ModelParameterCollection] =
    for {
      possibleStartingPoint <- startingPoint
      priorSample <-
        if (possibleStartingPoint == IndexedVectorCollection.empty) {
          samplePriors
        } else {
          Async[F].pure(possibleStartingPoint)
        }
      result <-
        runDynamicSimulationFrom(priorSample, warmUpSimulationCount, burnIn = true)
    } yield result

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  private[thylacine] lazy val launchInitialisation: F[Unit] =
    for {
      _            <- burnInComplete.set(false).commit
      _            <- epsilonAdjustmentResults.set(Queue()).commit
      _            <- simulationEpsilon.set(simulationInitialEpsilon).commit
      burninResult <- burnIn
      _ <- (for {
             _ <- currentlySampling.set(false)
             _ <- currentMcmcPosition.set(Option(burninResult))
             _ <- burnInComplete.set(true)
           } yield ()).commit
    } yield ()

  private[thylacine] val waitForInitialisationCompletion: F[Unit] =
    (for {
      burnInCompleted <- burnInComplete.get
      _               <- STM[F].waitFor(burnInCompleted)
    } yield ()).commit

  protected val getHmcmcSample: F[ModelParameterCollection] =
    for {
      position <- (secureWorkRight >> currentMcmcPosition.get).commit
      newSample <- setAndAcquireNewSample(position.get)
      _ <- currentlySampling.set(false).commit
    } yield newSample

  protected override val sampleModelParameters: F[ModelParameterCollection] =
    getHmcmcSample

  protected override val rawSampleModelParameters: F[VectorContainer] =
    sampleModelParameters.map(s => VectorContainer(modelParameterCollectionToRawVector(s)))

}
