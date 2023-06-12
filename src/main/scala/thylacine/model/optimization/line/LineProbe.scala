/*
 * Copyright 2020-2023 Greg von Nessi
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
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.{ModelParameterContext, ModelParameterPdf}

import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] trait LineProbe[F[_]] {
  this: AsyncImplicits[F] with ModelParameterPdf[F] with ModelParameterContext =>

  protected def lineProbeExpansionFactor: Double

  protected def probeLine(
      pt1: LineEvaluationResult,
      pt2: LineEvaluationResult,
      ordered: Boolean = false
  ): F[LineEvaluationTriple] = {
    val (lowerPoint, upperPoint) =
      if (ordered || pt1.result < pt2.result) {
        (pt1, pt2)
      } else {
        (pt2, pt1)
      }

    val probePointVector: Vector[Double] =
      upperPoint.vectorArgument.zip(lowerPoint.vectorArgument).map {
        case (upperPointCoordinate, lowerPointCoordinate) =>
          lineProbeExpansionFactor * (upperPointCoordinate - lowerPointCoordinate) + upperPointCoordinate
      }

    val probePointModelParameters: ModelParameterCollection =
      vectorValuesToModelParameterCollection(probePointVector)

    val probeEvaluationF: F[Double] = logPdfAt(probePointModelParameters)

    probeEvaluationF.flatMap { probeEvaluation =>
      val evaluationResult = LineEvaluationResult(result = probeEvaluation,
                                                  vectorArgument = probePointVector,
                                                  modelParameterArgument = probePointModelParameters
      )

      if (probeEvaluation >= upperPoint.result) {
        probeLine(upperPoint, evaluationResult, ordered = true)
      } else {
        Async[F].delay {
          LineEvaluationTriple(
            firstEndPoint = lowerPoint,
            middlePoint = upperPoint,
            secondEndPoint = evaluationResult
          )
        }
      }
    }
  }
}
