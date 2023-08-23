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
package thylacine.model.sampling.hmcmc

import thylacine.model.components.posterior.*
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.HmcmcTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{ IndexedVectorCollection, VectorContainer }
import thylacine.model.sampling.ModelParameterSampler
import thylacine.model.sampling.hmcmc.HmcmcEngine.*

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.syntax.all.*

/** Implementation of the Hamiltonian MCMC sampling algorithm
  */
private[thylacine] trait HmcmcEngine[F[_]] extends ModelParameterSampler[F] {
  this: AsyncImplicits[F] & Posterior[F, Prior[F, ?], ?] =>

  /*
   * - - -- --- ----- -------- -------------
   * Configuration
   * - - -- --- ----- -------- -------------
   */
  protected def simulationsBetweenSamples: Int
  protected def stepsInSimulation: Int
  protected def warmUpSimulationCount: Int
  protected def targetAcceptanceRatio: Double
  protected def startingPoint: F[ModelParameterCollection]
  protected def telemetryUpdateCallback: HmcmcTelemetryUpdate => F[Unit]

  /*
   * - - -- --- ----- -------- -------------
   * HMCMC
   * - - -- --- ----- -------- -------------
   */

  private def runLeapfrogAt(
    input: ModelParameterCollection,
    rawP: VectorContainer,
    gradNegLogPdf: ModelParameterCollection,
    simulationEpsilon: Double,
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
        runLeapfrogAt(xNew, pNewNew, gNew, simulationEpsilon, iterationCount + 1)
      }
    }

  private def getHamiltonianValue(p: VectorContainer, E: Double): Double =
    p.rawDotProductWith(p) / 2.0 + E

  private def runDynamicSimulationFrom(
    input: ModelParameterCollection,
    maxIterations: Int,
    logPdfOpt: Option[Double]                       = None,
    gradLogPdfOpt: Option[ModelParameterCollection] = None,
    burnIn: Boolean                                 = false,
    iterationCount: Int,
    numberOfRequestedSamples: Int,
    maxSimulationEpsilon: Double,
    minSimulationEpsilon: Double,
    simulationEpsilon: Double,
    jumpAcceptances: Int                   = 0,
    jumpAttempts: Int                      = 0,
    samples: Set[ModelParameterCollection] = Set()
  ): F[Set[ModelParameterCollection]] = {
    def simulationSpec: F[Set[ModelParameterCollection]] = (for {
      _ <- Async[F].ifM(Async[F].pure(burnIn))(
             Async[F].delay(print(s"\rHMCMC Sampling :: Burn-in Iteration - $iterationCount/$warmUpSimulationCount")),
             Async[F].unit
           )
      negLogPdf <- logPdfOpt match {
                     case Some(res) => Async[F].pure(res)
                     case _ => logPdfAt(input).map(_ * -1)
                   }
      gradNegLogPdf <- gradLogPdfOpt match {
                         case Some(res) => Async[F].pure(res)
                         case _ => logPdfGradientAt(input).map(_.rawScalarMultiplyWith(-1))
                       }
      p           <- Async[F].delay(VectorContainer.random(domainDimension))
      hamiltonian <- Async[F].delay(getHamiltonianValue(p, negLogPdf))
      xAndPNew    <- runLeapfrogAt(input, p, gradNegLogPdf, simulationEpsilon)
      (xNew, pNew) = xAndPNew
      eNew <- logPdfAt(xNew).map(_ * -1)
      hNew <- Async[F].delay(getHamiltonianValue(pNew, eNew))
      dH   <- Async[F].delay(hNew - hamiltonian)
      _ <- Async[F].ifM(Async[F].pure(burnIn || jumpAttempts <= 0))(
             Async[F].unit,
             telemetryUpdateCallback(
               HmcmcTelemetryUpdate(
                 samplesRemaining        = numberOfRequestedSamples - samples.size,
                 jumpAttempts            = jumpAttempts,
                 jumpAcceptances         = jumpAcceptances,
                 simulationDifferential  = simulationEpsilon,
                 hamiltonianDifferential = Option(dH)
               )
             ).start.void
           )
      result <- Async[F].ifM(Async[F].delay(dH < 0 || Math.random() < Math.exp(-dH)))(
                  for {
                    newGradNegLogPdf <- logPdfGradientAt(xNew).map(_.rawScalarMultiplyWith(-1))
                  } yield (xNew, eNew, newGradNegLogPdf, jumpAcceptances + 1, jumpAttempts + 1),
                  Async[F].delay((input, negLogPdf, gradNegLogPdf, jumpAcceptances, jumpAttempts + 1))
                )
    } yield result).flatMap { case (xNew, eNew, gNew, acceptances, attempts) =>
      runDynamicSimulationFrom(
        input                    = xNew,
        maxIterations            = maxIterations,
        logPdfOpt                = Option(eNew),
        gradLogPdfOpt            = Option(gNew),
        burnIn                   = burnIn,
        iterationCount           = iterationCount + 1,
        numberOfRequestedSamples = numberOfRequestedSamples,
        jumpAcceptances          = acceptances,
        jumpAttempts             = attempts,
        samples                  = samples,
        maxSimulationEpsilon     = maxSimulationEpsilon,
        minSimulationEpsilon     = minSimulationEpsilon,
        simulationEpsilon        = simulationEpsilon
      )
    }

    def testAcceptance = {
      val isOverRatio         = jumpAcceptances.toDouble / jumpAttempts > targetAcceptanceRatio
      val isLargerThanMinimum = simulationEpsilon > minSimulationEpsilon
      val isLargerThanMaximum = simulationEpsilon > maxSimulationEpsilon

      val newMaximumEpsilon =
        if (isLargerThanMaximum || !isOverRatio) {
          simulationEpsilon
        } else {
          maxSimulationEpsilon
        }

      val newMinimumEpsilon =
        if (!isLargerThanMinimum || isOverRatio) {
          simulationEpsilon
        } else {
          minSimulationEpsilon
        }

      val newEpsilon =
        if (isOverRatio && isLargerThanMaximum) {
          2 * simulationEpsilon
        } else if (isOverRatio) {
          (simulationEpsilon + maxSimulationEpsilon) / 2.0
        } else if (isLargerThanMinimum) {
          (simulationEpsilon + minSimulationEpsilon) / 2.0
        } else {
          simulationEpsilon / 2.0
        }

      (newMaximumEpsilon, newEpsilon, newMinimumEpsilon)
    }

    def sampleAppendSpec: F[Set[ModelParameterCollection]] =
      Async[F].delay(samples + input).flatMap { newSamples =>
        Async[F].ifM(Async[F].pure(newSamples.size >= numberOfRequestedSamples || burnIn))(
          Async[F].pure(newSamples),
          Async[F].delay(testAcceptance).flatMap { case (newMaxEpsilon, newEpsilon, newMinEpsilon) =>
            runDynamicSimulationFrom(
              input                    = input,
              maxIterations            = maxIterations,
              logPdfOpt                = logPdfOpt,
              gradLogPdfOpt            = gradLogPdfOpt,
              burnIn                   = burnIn,
              iterationCount           = 0,
              numberOfRequestedSamples = numberOfRequestedSamples,
              maxSimulationEpsilon     = newMaxEpsilon,
              minSimulationEpsilon     = newMinEpsilon,
              simulationEpsilon        = newEpsilon,
              samples                  = newSamples
            )
          }
        )
      }

    Async[F].ifM(Async[F].delay(iterationCount <= maxIterations))(
      simulationSpec,
      sampleAppendSpec
    )
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
      results <- runDynamicSimulationFrom(
                   input                    = priorSample,
                   maxIterations            = warmUpSimulationCount,
                   burnIn                   = true,
                   numberOfRequestedSamples = 1,
                   iterationCount           = 0,
                   maxSimulationEpsilon     = defaultMaxSimulationEpsilon,
                   minSimulationEpsilon     = defaultMinSimulationEpsilon,
                   simulationEpsilon        = defaultMaxSimulationEpsilon
                 )
      _ <- Async[F].delay(print(s"\nHMCMC Sampling :: Burn-in complete!\n"))
    } yield results.head

  /*
   * - - -- --- ----- -------- -------------
   * Framework Internal Interfaces
   * - - -- --- ----- -------- -------------
   */

  override protected def sampleModelParameters(numberOfSamples: Int): F[Set[ModelParameterCollection]] =
    for {
      startPt <- burnIn
      results <- runDynamicSimulationFrom(
                   input                    = startPt,
                   maxIterations            = simulationsBetweenSamples,
                   numberOfRequestedSamples = numberOfSamples,
                   iterationCount           = 0,
                   maxSimulationEpsilon     = defaultMaxSimulationEpsilon,
                   minSimulationEpsilon     = defaultMinSimulationEpsilon,
                   simulationEpsilon        = defaultMaxSimulationEpsilon
                 )
    } yield results

}

object HmcmcEngine {
  private val defaultMinSimulationEpsilon = 1e-20
  private val defaultMaxSimulationEpsilon = 1e-3
}
