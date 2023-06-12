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
package thylacine.model.core.computation

import thylacine.model.core.values._

import scala.annotation.unused

private[thylacine] case class FiniteDifferenceJacobian(
    private val evalAt: IndexedVectorCollection => VectorContainer,
    differential: Double
) {

  @unused
  private[thylacine] def finiteDifferenceJacobianAt(
      input: IndexedVectorCollection
  ): IndexedMatrixCollection = {
    val currentEvaluation = evalAt(input)
    val newMatrixCollectionMapping =
      input
        .rawNudgeComponents(differential)
        .toList
        .map { case (identifier, nudges) =>
          val gradientComponents = (1 to nudges.size)
            .zip(nudges)
            .toList
            .map { case (index, nudge) =>
              evalAt(nudge)
                .rawSubtract(currentEvaluation)
                .rawScalarProductWith(1 / differential)
                .values
                .map(k => (k._1, index) -> k._2)
            }

          identifier -> MatrixContainer(gradientComponents.reduce(_ ++ _), currentEvaluation.dimension, nudges.size)
        }
        .toMap

    IndexedMatrixCollection(newMatrixCollectionMapping)
  }

}
