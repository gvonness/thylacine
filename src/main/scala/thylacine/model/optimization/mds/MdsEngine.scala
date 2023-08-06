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

import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

trait MdsEngine[F[_]] extends ModelParameterOptimizer[F] {
  this: AsyncImplicits[F] with Posterior[F, Prior[F, _], _] =>

  protected def expansionMultiplier: Double

  protected def contractionMultiplier: Double

  protected def convergenceThreshold: Double

  protected def numberOfPriorSamplesToSetStartingPoint: Int

  protected def iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit]

  protected def isConvergedCallback: Unit => F[Unit]

  private def runEvaluation(indexAndPosition: (Int, ModelParameterCollection)): F[(Int, Double)] =
    for {
      result <- logPdfAt(indexAndPosition._2)
    } yield (indexAndPosition._1, result)

  private def processSimplexVertices(simplex: ModelParameterSimplex, indexToExclude: Int): F[(Int, Double)] =
    for {
      vertices   <- Async[F].delay((simplex.verticesAsModelParameters(this) - indexToExclude).toList)
      results    <- vertices.parTraverse(runEvaluation).map(_.toMap)
      bestResult <- Async[F].delay(results.maxBy(_._2))
    } yield bestResult

  private def evaluateSimplex(
    simplex: ModelParameterSimplex,
    best: (Int, Double)
  ): F[((Int, Double), ModelParameterSimplex)] = {
    (for {
      reflectedSimplex <- Async[F].delay(simplex.reflectAbout(best._1))
      newBest          <- processSimplexVertices(reflectedSimplex, best._1)
      bestAndSimplex <- Async[F].ifM(Async[F].delay(newBest._2 > best._2))(
                          for {
                            expandedSimplex <-
                              Async[F].delay(reflectedSimplex.expandAbout(best._1, expansionMultiplier))
                            expandedBest <- processSimplexVertices(expandedSimplex, best._1)
                            innerResult <- Async[F].ifM(Async[F].delay(expandedBest._2 > newBest._2))(
                                             Async[F].pure((expandedBest, expandedSimplex)),
                                             Async[F].pure((newBest, reflectedSimplex))
                                           )
                          } yield innerResult,
                          for {
                            contractedSimplex <-
                              Async[F].delay(reflectedSimplex.contractAbout(best._1, contractionMultiplier))
                            contractedBest <- processSimplexVertices(contractedSimplex, best._1)
                            innerResult <- Async[F].ifM(Async[F].delay(contractedBest._2 > best._2))(
                                             Async[F].pure((contractedBest, contractedSimplex)),
                                             Async[F].pure((best, contractedSimplex))
                                           )
                          } yield innerResult
                        )
      convergenceMeasure <- Async[F].delay(bestAndSimplex._2.maxAdjacentEdgeLength(bestAndSimplex._1._1))
      _ <- iterationUpdateCallback(
             OptimisationTelemetryUpdate(
               maxLogPdf    = bestAndSimplex._1._2,
               currentScale = convergenceMeasure,
               prefix       = "MDS"
             )
           ).start
    } yield (bestAndSimplex._2, bestAndSimplex._1, convergenceMeasure)).flatMap {
      case (simplex, best, convergenceMeasure) if convergenceMeasure <= convergenceThreshold =>
        isConvergedCallback(()).start.void >> Async[F].pure((best, simplex))
      case (simplex, best, _) =>
        evaluateSimplex(simplex, best)
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
      bestAndSimplex         <- evaluateSimplex(simplexAndStartingBest._1, simplexAndStartingBest._2)
    } yield (bestAndSimplex._1._2, bestAndSimplex._2.verticesAsModelParameters(this)(bestAndSimplex._1._1))
  }
}
