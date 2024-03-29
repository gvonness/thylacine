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
package thylacine.model.optimization.gradientdescent

import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.AsyncImplicits
import thylacine.model.core.telemetry.OptimisationTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.optimization.ModelParameterOptimizer
import thylacine.model.optimization.line.{ GoldenSectionSearch, LineEvaluationResult }
import thylacine.util.ScalaVectorOps.Implicits.*

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.syntax.all.*

trait ConjugateGradientEngine[F[_]] extends ModelParameterOptimizer[F] with GoldenSectionSearch[F] {
  this: AsyncImplicits[F] & Posterior[F, Prior[F, ?], ?] =>

  protected val telemetryPrefix: String = "Conjugate Gradient"

  protected def convergenceThreshold: Double

  protected def minimumNumberOfIterations: Int

  protected def iterationUpdateCallback: OptimisationTelemetryUpdate => F[Unit]

  protected def isConvergedCallback: Unit => F[Unit]

  private def calculateNextLogPdf(
    startingEvaluation: LineEvaluationResult,
    previousGradient: Vector[Double],
    previousSearchDirection: Vector[Double],
    iteration: Int
  ): F[(Double, ModelParameterCollection)] =
    (for {
      negativeGradientLogPdf <-
        logPdfGradientAt(startingEvaluation.modelParameterArgument).map(_.rawScalarMultiplyWith(-1))
      negativeGradientLogPdfVector     <- Async[F].delay(modelParameterCollectionToVectorValues(negativeGradientLogPdf))
      previousGradientMagnitudeSquared <- Async[F].delay(negativeGradientLogPdfVector.magnitudeSquared)
      newBeta <- Async[F].delay {
                   Math.max(
                     negativeGradientLogPdfVector
                       .subtract(previousGradient)
                       .dotProductWith(negativeGradientLogPdfVector) / previousGradientMagnitudeSquared,
                     0
                   )
                 }
      newDirection <-
        Async[F].delay(negativeGradientLogPdfVector.add(previousSearchDirection.scalarMultiplyWith(newBeta)))
      lineSearchResults <-
        searchDirectionAlong((startingEvaluation.result, startingEvaluation.vectorArgument), newDirection)
      lineSearchEvaluation <- Async[F].delay {
                                LineEvaluationResult(
                                  lineSearchResults._1,
                                  lineSearchResults._2,
                                  vectorValuesToModelParameterCollection(lineSearchResults._2)
                                )
                              }
    } yield (lineSearchEvaluation, negativeGradientLogPdfVector, newDirection)).flatMap {
      case (lineSearchEvaluation, negativeGradientLogPdfVector, newDirection) =>
        for {
          newDiff <- Async[F].delay(lineSearchEvaluation.result - startingEvaluation.result)
          _ <- iterationUpdateCallback(
                 OptimisationTelemetryUpdate(
                   maxLogPdf    = lineSearchEvaluation.result,
                   currentScale = newDiff,
                   prefix       = telemetryPrefix
                 )
               ).start
          result <-
            Async[F].ifM(Async[F].delay(newDiff > convergenceThreshold || iteration < minimumNumberOfIterations))(
              calculateNextLogPdf(
                lineSearchEvaluation,
                negativeGradientLogPdfVector,
                newDirection,
                iteration + 1
              ),
              isConvergedCallback(()).start >> Async[F].pure {
                (lineSearchEvaluation.result, lineSearchEvaluation.modelParameterArgument)
              }
            )
        } yield result
    }

  protected def calculateMaximumLogPdf(
    startingPt: ModelParameterCollection
  ): F[(Double, ModelParameterCollection)] =
    for {
      logPdf <- logPdfAt(startingPt)
      gradientLogPdfVector <- logPdfGradientAt(startingPt).map(nlpdfg =>
                                modelParameterCollectionToVectorValues(nlpdfg.rawScalarMultiplyWith(-1))
                              )
      startingPointVector <- Async[F].delay(modelParameterCollectionToVectorValues(startingPt))
      result <- calculateNextLogPdf(
                  LineEvaluationResult(logPdf, startingPointVector, startingPt),
                  gradientLogPdfVector,
                  gradientLogPdfVector,
                  1
                )
    } yield result

}
