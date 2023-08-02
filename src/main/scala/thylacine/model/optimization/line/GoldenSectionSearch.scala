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
package thylacine.model.optimization.line

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.values.modelparameters.{ ModelParameterContext, ModelParameterPdf }
import thylacine.model.optimization.line.GoldenSectionSearch.{ inversePhi, inversePhiSquared }
import thylacine.util.ScalaVectorOps.Implicits._

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.util.Random

private[thylacine] trait GoldenSectionSearch[F[_]] extends LineProbe[F] with LineSearch[F] {
  this: AsyncImplicits[F] with ModelParameterPdf[F] with ModelParameterContext =>

  protected def goldenSectionTolerance: Double

  private[thylacine] override final def searchColinearTriple(
    startPointEvaluation: (Double, Vector[Double]),
    midPointEvaluation: (Double, Vector[Double]),
    endPointEvaluation: (Double, Vector[Double])
  ): F[(Double, Vector[Double])] = {
    val bestNew: (Double, Vector[Double]) = if (startPointEvaluation._1 == endPointEvaluation._1) {
      Random.shuffle(List(startPointEvaluation, endPointEvaluation)).maxBy(_._1)
    } else {
      List(startPointEvaluation, endPointEvaluation).maxBy(_._1)
    }

    if (bestNew._1 > midPointEvaluation._1) {
      searchAlongLineJoining(midPointEvaluation, bestNew)
    } else {
      searchLineBetween(startPointEvaluation, endPointEvaluation)
    }
  }

  private def exploreLine(
    startPointEvaluation: (Double, Vector[Double]),
    direction: Vector[Double],
    probeDifferential: Double,
    directionNormalised: Boolean = false
  ): F[(Double, Vector[Double])] = {
    val normalisedDirection =
      if (directionNormalised) {
        direction
      } else {
        direction.scalarMultiplyWith(1.0 / direction.magnitude)
      }

    val forwardPoint = startPointEvaluation._2.add(normalisedDirection.scalarMultiplyWith(probeDifferential))
    val reversePoint = startPointEvaluation._2.add(normalisedDirection.scalarMultiplyWith(-probeDifferential))

    (for {
      forwardPointResult <- logPdfAt(vectorValuesToModelParameterCollection(forwardPoint))
      reversePointResult <- logPdfAt(vectorValuesToModelParameterCollection(reversePoint))
    } yield (forwardPointResult, reversePointResult)).flatMap {
      case (forwardPointResult, reversePointResult)
          if forwardPointResult == reversePointResult && startPointEvaluation._1 == forwardPointResult =>
        exploreLine(
          startPointEvaluation,
          normalisedDirection,
          probeDifferential * lineProbeExpansionFactor,
          directionNormalised = true
        )
      case (forwardPointResult, reversePointResult) =>
        searchColinearTriple(
          startPointEvaluation = (reversePointResult, reversePoint),
          midPointEvaluation   = startPointEvaluation,
          endPointEvaluation   = (forwardPointResult, forwardPoint)
        )
    }
  }

  private[thylacine] override final def searchDirectionAlong(
    startPointEvaluation: (Double, Vector[Double]),
    direction: Vector[Double]
  ): F[(Double, Vector[Double])] =
    exploreLine(startPointEvaluation, direction, goldenSectionTolerance / 10.0)

  private[thylacine] override final def searchAlongLineJoining(
    startPointEvaluation: (Double, Vector[Double]),
    endPointEvaluation: (Double, Vector[Double])
  ): F[(Double, Vector[Double])] = {
    val startEvaluation: LineEvaluationResult = LineEvaluationResult(
      startPointEvaluation._1,
      startPointEvaluation._2,
      vectorValuesToModelParameterCollection(startPointEvaluation._2)
    )

    val endEvaluation: LineEvaluationResult = LineEvaluationResult(
      endPointEvaluation._1,
      endPointEvaluation._2,
      vectorValuesToModelParameterCollection(endPointEvaluation._2)
    )

    for {
      probeResult         <- probeLine(startEvaluation, endEvaluation)
      goldenSectionResult <- goldenSectionSearch(probeResult.firstEndPoint, probeResult.secondEndPoint)
    } yield (goldenSectionResult.result, goldenSectionResult.vectorArgument)
  }

  private[thylacine] override final def searchLineBetween(
    startPointEvaluation: (Double, Vector[Double]),
    endPointEvaluation: (Double, Vector[Double])
  ): F[(Double, Vector[Double])] = {
    val startEvaluation: LineEvaluationResult = LineEvaluationResult(
      startPointEvaluation._1,
      startPointEvaluation._2,
      vectorValuesToModelParameterCollection(startPointEvaluation._2)
    )

    val endEvaluation: LineEvaluationResult = LineEvaluationResult(
      endPointEvaluation._1,
      endPointEvaluation._2,
      vectorValuesToModelParameterCollection(endPointEvaluation._2)
    )

    goldenSectionSearch(startEvaluation, endEvaluation).map { goldenSectionResult =>
      (goldenSectionResult.result, goldenSectionResult.vectorArgument)
    }
  }

  private def goldenSectionSearch(
    firstPoint: LineEvaluationResult,
    secondPoint: LineEvaluationResult,
    inputH: Option[Vector[Double]]                 = None,
    inputCEvaluation: Option[LineEvaluationResult] = None,
    inputDEvaluation: Option[LineEvaluationResult] = None
  ): F[LineEvaluationResult] = {

    val vectorDifference: Vector[Double] = inputH.getOrElse {
      secondPoint.vectorArgument.subtract(firstPoint.vectorArgument)
    }

    val vectorDifferenceMagnitude = vectorDifference.magnitude

    if (vectorDifferenceMagnitude <= goldenSectionTolerance) {
      Async[F].delay {
        if (firstPoint.result >= secondPoint.result) {
          firstPoint
        } else {
          secondPoint
        }
      }
    } else {
      val newH: Vector[Double] = vectorDifference.scalarMultiplyWith(inversePhi)

      lazy val newFirstPointVectorArgument =
        firstPoint.vectorArgument.add(vectorDifference.scalarMultiplyWith(inversePhiSquared))

      lazy val newSecondPointVectorArgument: Vector[Double] =
        firstPoint.vectorArgument.add(newH)

      val newCEvaluationF = inputCEvaluation.map(Async[F].pure).getOrElse {
        val modelParameters = vectorValuesToModelParameterCollection(newFirstPointVectorArgument)

        logPdfAt(modelParameters).map { evaluationResult =>
          LineEvaluationResult(
            result                 = evaluationResult,
            vectorArgument         = newFirstPointVectorArgument,
            modelParameterArgument = modelParameters
          )
        }
      }

      val newDEvaluationF = inputDEvaluation.map(Async[F].pure).getOrElse {
        val modelParameters = vectorValuesToModelParameterCollection(newSecondPointVectorArgument)

        logPdfAt(modelParameters).map { evaluationResult =>
          LineEvaluationResult(
            result                 = evaluationResult,
            vectorArgument         = newSecondPointVectorArgument,
            modelParameterArgument = modelParameters
          )
        }
      }

      (for {
        newCEvaluation <- newCEvaluationF
        newDEvaluation <- newDEvaluationF
      } yield (newCEvaluation, newDEvaluation)).flatMap {
        case (newCEvaluation, newDEvaluation) if newCEvaluation.result > newDEvaluation.result =>
          goldenSectionSearch(
            firstPoint       = firstPoint,
            secondPoint      = newDEvaluation,
            inputH           = Some(newH),
            inputCEvaluation = None,
            inputDEvaluation = Some(newCEvaluation)
          )
        case (newCEvaluation, newDEvaluation) =>
          goldenSectionSearch(
            firstPoint       = newCEvaluation,
            secondPoint      = secondPoint,
            inputH           = Some(newH),
            inputCEvaluation = Some(newDEvaluation),
            inputDEvaluation = None
          )
      }

    }
  }
}

private[thylacine] object GoldenSectionSearch {
  private[thylacine] val inversePhi        = (Math.sqrt(5) - 1) / 2.0
  private[thylacine] val inversePhiSquared = (3 - Math.sqrt(5)) / 2.0
}
