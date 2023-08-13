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
package thylacine.model.optimization.hookeandjeeves

import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer
import thylacine.util.MathOps

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.syntax.all.*

import scala.util.Random
import scala.Vector as ScalaVector

private[thylacine] trait HookeAndJeevesEngine[F[_]] extends ModelParameterOptimizer[F] {
  this: AsyncImplicits[F] & Posterior[F, Prior[F, ?], ?] =>

  protected val telemetryPrefix: String = "Hooke and Jeeves"
  protected def convergenceThreshold: Double
  protected def numberOfSamplesToSetScale: Int

  protected def iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit]

  protected def isConvergedCallback: Unit => F[Unit]

  private def findMaxDimensionalDifference(input: List[Vector[Double]]): Double =
    input.tail
      .foldLeft(input.head.zip(input.head)) { (i, j) =>
        i.zip(j).map { k =>
          (if (k._2 > k._1._1) k._2 else k._1._1, if (k._2 < k._1._2) k._2 else k._1._2)
        }
      }
      .map(i => i._1 - i._2)
      .max

  // Can parallel traverse, as there probably isn't much else going on
  private def initialize(
    numberOfPriorSamples: Int
  ): F[(Double, (Double, ScalaVector[Double]))] =
    for {
      samples <- (1 to numberOfPriorSamples).toList.parTraverse(_ => samplePriors)
      samplesRaw <-
        samples.parTraverse(s => Async[F].delay(modelParameterCollectionToVectorValues(s)))
      bestSample <-
        samples
          .zip(samplesRaw)
          .parTraverse(s => logPdfAt(s._1).map(lpdf => (lpdf, s._2)))
          .map(_.maxBy(_._1))
      maxDifference <- Async[F].delay(findMaxDimensionalDifference(samplesRaw))
    } yield (maxDifference, bestSample)

  protected def nudgeAndEvaluate(
    index: Int,
    nudgeAmount: Double,
    input: ScalaVector[Double]
  ): F[(Double, ScalaVector[Double])] =
    for {
      nudgedRawVector <- Async[F].delay(MathOps.modifyVectorIndex(input)(index, _ + nudgeAmount))
      nudgedVector <-
        Async[F].delay(vectorValuesToModelParameterCollection(nudgedRawVector))
      logPdf <- logPdfAt(nudgedVector)
    } yield (logPdf, nudgedRawVector)

  protected def dimensionScan(
    nudgeAmount: Double,
    startingPoint: ScalaVector[Double],
    startingLogPdf: Double
  ): F[(Double, ScalaVector[Double])] =
    Random
      .shuffle(startingPoint.indices.toList)
      .foldLeft(Async[F].pure((startingLogPdf, startingPoint))) { case (previousF, testIndex) =>
        previousF.flatMap { case previous @ (_, currentArgMax) =>
          List(-nudgeAmount, nudgeAmount)
            .traverse(nudgeAndEvaluate(testIndex, _, currentArgMax))
            .map { results =>
              (previous +: Random.shuffle(results)).maxBy(_._1)
            }
        }
      }

  private def calculateNextLogPdf(
    currentScale: Double,
    currentBest: (Double, ScalaVector[Double])
  ): F[(Double, ScalaVector[Double])] =
    (for {
      scanResult <-
        dimensionScan(currentScale, currentBest._2, currentBest._1)
      scaleAndConverged <- Async[F].ifM(Async[F].delay(scanResult._1 > currentBest._1))(
                             iterationUpdateCallback(
                               OptimisationTelemetryUpdate(
                                 maxLogPdf    = scanResult._1,
                                 currentScale = currentScale,
                                 prefix       = telemetryPrefix
                               )
                             ).start >> Async[F].pure((currentScale, false)),
                             Async[F].ifM(Async[F].delay(currentScale > convergenceThreshold))(
                               for {
                                 scaleResult <- Async[F].delay(currentScale / 2.0)
                                 _ <- iterationUpdateCallback(
                                        OptimisationTelemetryUpdate(
                                          maxLogPdf    = scanResult._1,
                                          currentScale = scaleResult,
                                          prefix       = telemetryPrefix
                                        )
                                      ).start
                               } yield (scaleResult, false),
                               isConvergedCallback(()).start >> Async[F].pure((currentScale, true))
                             )
                           )
    } yield (scaleAndConverged._1, scanResult, scaleAndConverged._2)).flatMap {
      case (scale, result, isConverged) if !isConverged =>
        calculateNextLogPdf(scale, result)
      case _ =>
        Async[F].pure(currentBest)
    }

  override protected def calculateMaximumLogPdf(
    startPt: ModelParameterCollection
  ): F[(Double, ModelParameterCollection)] =
    for {
      initScaleAndBest <- initialize(numberOfSamplesToSetScale)
      selectedBest <- Async[F].ifM(Async[F].delay(startPt.index.isEmpty))(
                        Async[F].pure(initScaleAndBest._2),
                        for {
                          pt     <- Async[F].delay(modelParameterCollectionToVectorValues(startPt))
                          logPdf <- logPdfAt(startPt)
                        } yield (logPdf, pt)
                      )
      result <- calculateNextLogPdf(initScaleAndBest._1, selectedBest)
    } yield (result._1, vectorValuesToModelParameterCollection(result._2))

}
