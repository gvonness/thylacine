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
package thylacine.model.optimization.mds

import bengal.stm._
import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

trait MdsEngine[F[_]] extends ModelParameterOptimizer[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  protected def expansionMultiplier: Double

  protected def contractionMultiplier: Double

  protected def convergenceThreshold: Double

  protected def numberOfPriorSamplesToSetStartingPoint: Int

  protected def iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit]

  protected def isConvergedCallback: Unit => F[Unit]

  protected val currentBest: TxnVar[F, (Int, Double)]

  protected val currentSimplex: TxnVar[F, ModelParameterSimplex]

  protected val isConverged: TxnVar[F, Boolean]

  private def runEvaluation(indexAndPosition: (Int, ModelParameterCollection)): F[(Int, Double)] =
    for {
      result <- logPdfAt(indexAndPosition._2)
    } yield (indexAndPosition._1, result)

  private def recordBest(bestResult: (Int, Double)): Txn[Unit] =
    for {
      best <- currentBest.get
      _ <- if (bestResult._2 > best._2) {
             currentBest.set(bestResult)
           } else {
             STM[F].unit
           }
    } yield ()

  private def processSimplexVertices(simplex: ModelParameterSimplex, indexToExclude: Int): F[Unit] =
    for {
      vertices   <- Async[F].delay((simplex.verticesAsModelParameters(this) - indexToExclude).toList)
      results    <- vertices.parTraverse(runEvaluation).map(_.toMap)
      bestResult <- Async[F].delay(results.maxBy(_._2))
      _          <- recordBest(bestResult).commit
    } yield ()

  private val evaluateSimplex: F[Double] = {
    for {
      simplex          <- currentSimplex.get.commit
      best             <- currentBest.get.commit
      reflectedSimplex <- Async[F].delay(simplex.reflectAbout(best._1))
      _                <- currentSimplex.set(reflectedSimplex).commit
      _                <- processSimplexVertices(reflectedSimplex, best._1)
      newBest          <- currentBest.get.commit
      _ <- if (newBest._2 > best._2) {
             for {
               expandedSimplex <- Async[F].delay(reflectedSimplex.expandAbout(best._1, expansionMultiplier))
               _               <- processSimplexVertices(expandedSimplex, best._1)
               expandedBest    <- currentBest.get.commit
               _ <- if (expandedBest._2 > newBest._2) {
                      currentSimplex.set(expandedSimplex).commit
                    } else {
                      Async[F].unit
                    }
             } yield ()
           } else {
             for {
               contractedSimplex <- Async[F].delay(reflectedSimplex.contractAbout(best._1, contractionMultiplier))
               _                 <- processSimplexVertices(contractedSimplex, best._1)
               _                 <- currentSimplex.set(contractedSimplex).commit
             } yield ()
           }
      finalSimplex       <- currentSimplex.get.commit
      finalBest          <- currentBest.get.commit
      convergenceMeasure <- Async[F].delay(finalSimplex.maxAdjacentEdgeLength(finalBest._1))
      _ <- iterationUpdateCallback(
             OptimisationTelemetryUpdate(maxLogPdf = finalBest._2, currentScale = convergenceMeasure, prefix = "MDS")
           ).start
    } yield convergenceMeasure
  }

  private val simplexRecursion: F[Unit] =
    evaluateSimplex.flatMap { convergenceMeasure =>
      if (convergenceMeasure <= convergenceThreshold) {
        isConverged.set(true).commit >> isConvergedCallback(()).start.void
      } else {
        simplexRecursion
      }
    }

  private def getStartingPoint(
    numberOfPriorSamples: Int
  ): F[ModelParameterCollection] =
    for {
      samples <- (1 to numberOfPriorSamples).toList.parTraverse(_ => samplePriors)
      samplesRaw <-
        samples.parTraverse(s => Async[F].delay(modelParameterCollectionToVectorValues(s)))
      bestSample <-
        samples
          .zip(samplesRaw)
          .parTraverse(s => logPdfAt(s._1).map(lpdf => (lpdf, s._2)))
          .map(_.maxBy(_._1))
    } yield vectorValuesToModelParameterCollection(bestSample._2)

  private def processStartingPoint(startPt: ModelParameterCollection): F[ModelParameterCollection] =
    if (startPt.index.isEmpty) {
      getStartingPoint(numberOfPriorSamplesToSetStartingPoint)
    } else {
      Async[F].pure(startPt)
    }

  override protected def calculateMaximumLogPdf(
    startPt: ModelParameterCollection
  ): F[(Double, ModelParameterCollection)] = {
    // Do a parallel traversal here, as there is probably
    // not going to be much else going on at this stage
    def startingBestF(input: ModelParameterCollection): F[(ModelParameterSimplex, (Int, Double))] =
      for {
        simplex <- Async[F].delay(ModelParameterSimplex.unitRegularCenteredOn(input, this))
        bestIndexAndValue <- simplex
                               .verticesAsModelParameters(this)
                               .toList
                               .parTraverse { case (index, position) =>
                                 logPdfAt(position).map((index, _))
                               }
                               .map(_.maxBy(_._2))
      } yield (simplex, bestIndexAndValue)

    for {
      processedStartingPoint <- processStartingPoint(startPt)
      simplexAndStartingBest <- startingBestF(processedStartingPoint)
      _ <- (for {
             _ <- isConverged.set(false)
             _ <- currentBest.set(simplexAndStartingBest._2)
             _ <- currentSimplex.set(simplexAndStartingBest._1)
           } yield ()).commit
      _ <- simplexRecursion.start.void
      _ <- (for {
             converged <- isConverged.get
             _         <- STM[F].waitFor(converged)
           } yield ()).commit
      currentBest    <- currentBest.get.commit
      currentSimplex <- currentSimplex.get.commit
    } yield (currentBest._2, currentSimplex.verticesAsModelParameters(this)(currentBest._1))
  }
}
