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

import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.util.Random
import scala.{Vector => ScalaVector}

private[thylacine] trait HookeAndJeevesEngine[F[_]] extends ModelParameterOptimizer[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>
  protected def convergenceThreshold: Double
  protected def numberOfSamplesToSetScale: Int

  protected def newMaximumCallback: Double => Unit
  protected def newScaleCallback: Double => Unit
  protected def isConvergedCallback: Unit => Unit

  protected val currentBest: TxnVar[F, (Double, ScalaVector[Double])]

  protected val currentScale: TxnVar[F, Double]

  protected val isConverged: TxnVar[F, Boolean]

  private def findMaxDimensionalDifference(input: List[Vector[Double]]): Double =
    input.tail
      .foldLeft(input.head.zip(input.head)) { (i, j) =>
        i.zip(j).map { k =>
          (if (k._2 > k._1._1) k._2 else k._1._1, if (k._2 < k._1._2) k._2 else k._1._2)
        }
      }
      .map(i => i._1 - i._2)
      .max

  private def initialize(
      numberOfPriorSamples: Int
  ): ResultOrErrF[F, Unit] =
    for {
      samples <- (1 to numberOfPriorSamples).toList.parTraverse(_ => samplePriors)
      samplesRaw <-
        samples.parTraverse(modelParameterCollectionToVectorValues)
      bestSample <-
        samples
          .zip(samplesRaw)
          .parTraverse(s => logPdfAt(s._1).map(lpdf => (lpdf, s._2)))
          .map(_.maxBy(_._1))
      maxDifference = findMaxDimensionalDifference(samplesRaw)
      _ <- (for {
             _ <- currentScale.set(maxDifference)
             _ <- currentBest.set(bestSample)
             _ <- isConverged.set(false)
           } yield ()).commit.toResultM
    } yield ()

  private def nudgeAndEvaluate(
      index: Int,
      nudgeAmount: Double,
      input: ScalaVector[Double]
  ): ResultOrErrF[F, (Double, ScalaVector[Double])] =
    for {
      splitVector <- input.splitAt(index).toResultM
      nudgedRawVector <- (splitVector._1 ++ Vector(
                           splitVector._2.head + nudgeAmount
                         ) ++ splitVector._2.tail).toResultM
      nudgedVector <-
        vectorValuesToModelParameterCollection(nudgedRawVector)
      logPdf <- logPdfAt(nudgedVector)
    } yield (logPdf, nudgedRawVector)

  private def dimensionScan(
      nudgeAmount: Double,
      startingPoint: ScalaVector[Double],
      startingLogPdf: Double
  ): ResultOrErrF[F, (Double, ScalaVector[Double])] = {
    val nudges: List[Double] = List(-nudgeAmount, nudgeAmount)

    Random
      .shuffle(startingPoint.indices.toList)
      .foldLeft((startingLogPdf, startingPoint).toResultM) { (i, j) =>
        for {
          prev    <- i
          results <- nudges.parTraverse(nudgeAndEvaluate(j, _, prev._2))
        } yield (prev +: Random.shuffle(results)).maxBy(_._1)
      }
  }

  private def runDimensionalIteration: ResultOrErrF[F, Unit] =
    for {
      scaleAndBest <- (for {
                        scale <- currentScale.get
                        best  <- currentBest.get
                      } yield (scale, best)).commit.toResultM
      scanResult <-
        dimensionScan(scaleAndBest._1, scaleAndBest._2._2, scaleAndBest._2._1)
      unwrappedResult = if (scanResult._1 > scaleAndBest._2._1) {
                          currentBest.set(scanResult).commit >> Async[F].delay(
                            newMaximumCallback(scanResult._1)
                          )
                        } else if (scaleAndBest._1 > convergenceThreshold) {
                          (for {
                            scale <- currentScale.get
                            newScale = scale / 2
                            _ <- currentScale.set(newScale)
                          } yield newScale).commit.map(newScaleCallback)
                        } else {
                          isConverged.set(true).commit >> Async[F].delay(isConvergedCallback)
                        }
      _ <- unwrappedResult.toResultM
    } yield ()

  private def optimisationRecursion: ResultOrErrF[F, Unit] =
    runDimensionalIteration.flatMap { _ =>
      for {
        converged <- isConverged.get.commit.toResultM
        _         <- if (!converged) optimisationRecursion else ResultOrErrF.unit
      } yield ()
    }

  override protected def calculateMaximumLogPdf(
      startPt: ModelParameterCollection[F]
  ): ResultOrErrF[F, (Double, ModelParameterCollection[F])] =
    for {
      _ <- initialize(numberOfSamplesToSetScale)
      _ <- if (startPt.index.isEmpty) {
             ResultOrErrF.unit
           } else {
             for {
               pt     <- modelParameterCollectionToVectorValues(startPt)
               logPdf <- logPdfAt(startPt)
               _      <- currentBest.set((logPdf, pt)).commit.toResultM
             } yield ()
           }
      _          <- optimisationRecursion
      best       <- currentBest.get.commit.toResultM
      bestParams <- vectorValuesToModelParameterCollection(best._2)
    } yield (best._1, bestParams)

}
