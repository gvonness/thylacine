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
package thylacine.model.optimization.line

import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

private[thylacine] case class LineEvaluationResult(
  private[thylacine] val result: Double,
  private[thylacine] val vectorArgument: Vector[Double],
  private[thylacine] val modelParameterArgument: ModelParameterCollection
)

private[thylacine] object LineEvaluationResult {
  private[thylacine] def apply(
    input: (Double, Vector[Double])
  )(vectorValuesToModelParameterCollection: Vector[Double] => ModelParameterCollection): LineEvaluationResult =
    LineEvaluationResult(
      input._1,
      input._2,
      vectorValuesToModelParameterCollection(input._2)
    )
}
