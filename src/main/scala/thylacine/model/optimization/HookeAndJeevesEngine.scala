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
package thylacine.model.optimization

import bengal.stm._
import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.Erratum.ResultOrErrIo
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.ModelParameterOptimizer

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

import scala.util.Random
import scala.{Vector => ScalaVector}

private[thylacine] abstract class HookeAndJeevesEngine(implicit stm: STM[IO]) extends ModelParameterOptimizer {

  import stm._

  protected def posterior: Posterior[Prior[_], _]
  protected def convergenceThreshold: Double
  protected def numberOfSamplesToSetScale: Int

  protected def newMaximumCallback: Double => Unit
  protected def newScaleCallback: Double => Unit
  protected def isConvergedCallback: Unit => Unit

  private val currentBest: TxnVar[(Double, ScalaVector[Double])] =
    TxnVar.of((0d, ScalaVector[Double]())).unsafeRunSync()

  private val currentScale: TxnVar[Double] =
    TxnVar.of(0d).unsafeRunSync()

  private val isConverged: TxnVar[Boolean] =
    TxnVar.of(false).unsafeRunSync()

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
  ): ResultOrErrIo[Unit] =
    for {
      samples <- (1 to numberOfPriorSamples).toList.parTraverse(_ => posterior.samplePriors)
      samplesRaw <-
        samples.parTraverse(posterior.modelParameterCollectionToVectorValues)
      bestSample <-
        samples
          .zip(samplesRaw)
          .parTraverse(s => posterior.logPdfAt(s._1).map(lpdf => (lpdf, s._2)))
          .map(_.maxBy(_._1))
      maxDifference = findMaxDimensionalDifference(samplesRaw)
      _ <- ResultOrErrIo.fromIo {
             (for {
               _ <- currentScale.set(maxDifference)
               _ <- currentBest.set(bestSample)
               _ <- isConverged.set(false)
             } yield ()).commit
           }
    } yield ()

  private def nudgeAndEvaluate(
      index: Int,
      nudgeAmount: Double,
      input: ScalaVector[Double]
  ): ResultOrErrIo[(Double, ScalaVector[Double])] =
    for {
      splitVector <- ResultOrErrIo.fromCalculation(input.splitAt(index))
      nudgedRawVector <- ResultOrErrIo.fromCalculation(
                           splitVector._1 ++ Vector(
                             splitVector._2.head + nudgeAmount
                           ) ++ splitVector._2.tail
                         )
      nudgedVector <-
        posterior.vectorValuesToModelParameterCollection(nudgedRawVector)
      logPdf <- posterior.logPdfAt(nudgedVector)
    } yield (logPdf, nudgedRawVector)

  private def dimensionScan(
      nudgeAmount: Double,
      startingPoint: ScalaVector[Double],
      startingLogPdf: Double
  ): ResultOrErrIo[(Double, ScalaVector[Double])] = {
    val nudges: List[Double] = List(-nudgeAmount, nudgeAmount)

    Random
      .shuffle(startingPoint.indices.toList)
      .foldLeft(ResultOrErrIo.fromValue((startingLogPdf, startingPoint))) { (i, j) =>
        for {
          prev    <- i
          results <- nudges.parTraverse(nudgeAndEvaluate(j, _, prev._2))
        } yield (prev +: Random.shuffle(results)).maxBy(_._1)
      }
  }

  private def runDimensionalIteration: ResultOrErrIo[Unit] =
    for {
      scaleAndBest <- ResultOrErrIo.fromIo {
                        (for {
                          scale <- currentScale.get
                          best  <- currentBest.get
                        } yield (scale, best)).commit
                      }
      scanResult <-
        dimensionScan(scaleAndBest._1, scaleAndBest._2._2, scaleAndBest._2._1)
      _ <- ResultOrErrIo.fromIo {
             if (scanResult._1 > scaleAndBest._2._1) {
               currentBest.set(scanResult).commit >> IO(
                 newMaximumCallback(scanResult._1)
               )
             } else if (scaleAndBest._1 > convergenceThreshold) {
               (for {
                 scale <- currentScale.get
                 newScale = scale / 2
                 _ <- currentScale.set(newScale)
               } yield newScale).commit.map(newScaleCallback)
             } else {
               isConverged.set(true).commit >> IO(isConvergedCallback)
             }
           }
    } yield ()

  private def optimisationRecursion: ResultOrErrIo[Unit] =
    runDimensionalIteration.flatMap { _ =>
      for {
        converged <- ResultOrErrIo.fromIo(isConverged.get.commit)
        _         <- if (!converged) optimisationRecursion else ResultOrErrIo.unit
      } yield ()
    }

  override protected def calculateMaximumLogPdf(
      startPt: ModelParameterCollection
  ): ResultOrErrIo[(Double, ModelParameterCollection)] =
    for {
      _ <- initialize(numberOfSamplesToSetScale)
      _ <- if (startPt.index.isEmpty) {
             ResultOrErrIo.unit
           } else {
             for {
               pt     <- posterior.modelParameterCollectionToVectorValues(startPt)
               logPdf <- posterior.logPdfAt(startPt)
               _      <- ResultOrErrIo.fromIo(currentBest.set((logPdf, pt)).commit)
             } yield ()
           }
      _          <- optimisationRecursion
      best       <- ResultOrErrIo.fromIo(currentBest.get.commit)
      bestParams <- posterior.vectorValuesToModelParameterCollection(best._2)
    } yield (best._1, bestParams)

}
