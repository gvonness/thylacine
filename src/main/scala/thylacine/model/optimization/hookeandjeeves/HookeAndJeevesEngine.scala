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
package thylacine.model.optimization.hookeandjeeves

import bengal.stm.STM
import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer
import thylacine.util.MathOps

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.util.Random
import scala.{Vector => ScalaVector}

private[thylacine] trait HookeAndJeevesEngine[F[_]] extends ModelParameterOptimizer[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  protected val telemetryPrefix: String = "Hooke and Jeeves"
  protected def convergenceThreshold: Double
  protected def numberOfSamplesToSetScale: Int

  protected def iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit]

  protected def isConvergedCallback: Unit => F[Unit]

  protected val currentBest: TxnVar[F, (Double, ScalaVector[Double])]

  protected val currentScale: TxnVar[F, Double]

  protected val isConverged: TxnVar[F, Boolean]

  protected def findMaxDimensionalDifference(input: List[Vector[Double]]): Double =
    input.tail
      .foldLeft(input.head.zip(input.head)) { (i, j) =>
        i.zip(j).map { k =>
          (if (k._2 > k._1._1) k._2 else k._1._1, if (k._2 < k._1._2) k._2 else k._1._2)
        }
      }
      .map(i => i._1 - i._2)
      .max

  // Can parallel traverse, as there probably isn't much else going on
  protected def initialize(
      numberOfPriorSamples: Int
  ): F[Unit] =
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
      _ <- (for {
             _ <- currentScale.set(maxDifference)
             _ <- currentBest.set(bestSample)
             _ <- isConverged.set(false)
           } yield ()).commit
    } yield ()

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

  protected val runDimensionalIteration: F[Unit] =
    for {
      scaleAndBest <- (for {
                        scale <- currentScale.get
                        best  <- currentBest.get
                      } yield (scale, best)).commit
      scanResult <-
        dimensionScan(scaleAndBest._1, scaleAndBest._2._2, scaleAndBest._2._1)
      _ <- Async[F].ifM(Async[F].delay(scanResult._1 > scaleAndBest._2._1))(
             for {
               _     <- currentBest.set(scanResult).commit
               scale <- currentScale.get.commit
               _ <- iterationUpdateCallback(
                      OptimisationTelemetryUpdate(maxLogPdf = scanResult._1,
                                                  currentScale = scale,
                                                  prefix = telemetryPrefix
                      )
                    ).start
             } yield (),
             Async[F].ifM(Async[F].delay(scaleAndBest._1 > convergenceThreshold))(
               for {
                 scaleResult <- (for {
                                  scale    <- currentScale.get
                                  newScale <- STM[F].delay(scale / 2)
                                  _        <- currentScale.set(newScale)
                                } yield newScale).commit
                 maxLogPdf <- currentBest.get.commit
                 _ <- iterationUpdateCallback(
                        OptimisationTelemetryUpdate(maxLogPdf = maxLogPdf._1,
                                                    currentScale = scaleResult,
                                                    prefix = telemetryPrefix
                        )
                      ).start
               } yield (),
               isConverged.set(true).commit >> isConvergedCallback(()).start.void
             )
           )
    } yield ()

  protected val optimisationRecursion: F[Unit] =
    runDimensionalIteration.flatMap { _ =>
      for {
        converged <- isConverged.get.commit
        _         <- if (!converged) optimisationRecursion else Async[F].unit
      } yield ()
    }

  override protected def calculateMaximumLogPdf(
      startPt: ModelParameterCollection
  ): F[(Double, ModelParameterCollection)] =
    for {
      _ <- initialize(numberOfSamplesToSetScale)
      _ <- if (startPt.index.isEmpty) {
             Async[F].unit
           } else {
             for {
               pt     <- Async[F].delay(modelParameterCollectionToVectorValues(startPt))
               logPdf <- logPdfAt(startPt)
               _      <- currentBest.set((logPdf, pt)).commit
             } yield ()
           }
      _    <- optimisationRecursion
      best <- currentBest.get.commit
    } yield (best._1, vectorValuesToModelParameterCollection(best._2))

}
