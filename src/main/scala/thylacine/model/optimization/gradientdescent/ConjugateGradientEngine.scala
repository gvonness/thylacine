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
package thylacine.model.optimization.gradientdescent

import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer
import thylacine.model.optimization.line.{GoldenSectionSearch, LineEvaluationResult}
import thylacine.util.ScalaVectorOps.Implicits._

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

import scala.collection.immutable.Queue

trait ConjugateGradientEngine[F[_]] extends ModelParameterOptimizer[F] with GoldenSectionSearch[F] {
  this: AsyncImplicits[F] with Posterior[F, Prior[F, _], _] =>

  protected def convergenceThreshold: Double

  protected def numberOfResultsToRetain: Int

  protected def newMaximumCallback: Double => F[Unit]

  protected def isConvergedCallback: Unit => F[Unit]

  private def calculateNextLogPdf(
      startingEvaluation: LineEvaluationResult,
      previousGradient: Vector[Double],
      previousSearchDirection: Vector[Double],
      previousResults: Queue[Double]
  ): F[(Double, ModelParameterCollection)] =
    (for {
      gradientLogPdf                   <- logPdfGradientAt(startingEvaluation.modelParameterArgument)
      gradientLogPdfVector             <- Async[F].delay(modelParameterCollectionToVectorValues(gradientLogPdf))
      previousGradientMagnitudeSquared <- Async[F].delay(gradientLogPdfVector.magnitudeSquared)
      newBeta <- Async[F].delay {
                   Math.max(
                     gradientLogPdfVector
                       .subtract(previousGradient)
                       .dotProductWith(gradientLogPdfVector) / previousGradientMagnitudeSquared,
                     0
                   )
                 }
      newDirection <- Async[F].delay {
                        if (newBeta == 0) {
                          gradientLogPdfVector
                        } else {
                          gradientLogPdfVector.add(previousSearchDirection.scalarMultiplyWith(newBeta))
                        }
                      }
      lineSearchResults <-
        searchDirectionAlong((startingEvaluation.result, startingEvaluation.vectorArgument), newDirection)
      lineSearchEvaluation <- Async[F].delay {
                                LineEvaluationResult(
                                  lineSearchResults._1,
                                  lineSearchResults._2,
                                  vectorValuesToModelParameterCollection(lineSearchResults._2)
                                )
                              }
    } yield (lineSearchEvaluation, gradientLogPdfVector, newDirection)).flatMap {
      case (lineSearchEvaluation, gradientLogPdfVector, newDirection)
          if previousResults.size < numberOfResultsToRetain =>
        (if (previousResults.isEmpty || lineSearchEvaluation.result > previousResults.dequeue._1) {
           newMaximumCallback(lineSearchEvaluation.result)
         } else {
           Async[F].unit
         }) >> calculateNextLogPdf(
          lineSearchEvaluation,
          gradientLogPdfVector,
          newDirection,
          previousResults.enqueue(lineSearchEvaluation.result)
        )
      case (lineSearchEvaluation, gradientLogPdfVector, newDirection)
          if Math.abs(
            previousResults.dequeue._1 - lineSearchEvaluation.result
          ) > convergenceThreshold =>
        (if (lineSearchEvaluation.result > previousResults.dequeue._1) {
           newMaximumCallback(lineSearchEvaluation.result)
         } else {
           Async[F].unit
         }) >> calculateNextLogPdf(
          lineSearchEvaluation,
          gradientLogPdfVector,
          newDirection,
          previousResults.enqueue(lineSearchEvaluation.result).dequeue._2
        )
      case (lineSearchEvaluation, _, _) =>
        isConvergedCallback(()).start >> Async[F].pure {
          (lineSearchEvaluation.result, lineSearchEvaluation.modelParameterArgument)
        }
    }

  protected def calculateMaximumLogPdf(
      startingPt: ModelParameterCollection
  ): F[(Double, ModelParameterCollection)] =
    for {
      logPdf               <- logPdfAt(startingPt)
      gradientLogPdfVector <- logPdfGradientAt(startingPt).map(modelParameterCollectionToVectorValues)
      startingPointVector  <- Async[F].delay(modelParameterCollectionToVectorValues(startingPt))
      result <- calculateNextLogPdf(LineEvaluationResult(logPdf, startingPointVector, startingPt),
                                    gradientLogPdfVector,
                                    gradientLogPdfVector,
                                    Queue[Double]()
                )
    } yield result

}
