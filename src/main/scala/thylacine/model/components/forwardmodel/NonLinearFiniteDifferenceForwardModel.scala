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
package thylacine.model.components.forwardmodel

import thylacine.model.core.Erratum.ResultOrErrIo
import thylacine.model.core.GenericIdentifier.ModelParameterIdentifier
import thylacine.model.core.{IndexedVectorCollection, VectorContainer}

case class NonLinearFiniteDifferenceForwardModel(
    evaluation: Map[String, Vector[Double]] => Vector[Double],
    domainDimensions: Map[String, Int],
    override val rangeDimension: Int,
    override val differential: Double,
    override val validated: Boolean = false
) extends FiniteDifferenceForwardModel {

  override protected val orderedParameterIdentifiersWithDimension
      : ResultOrErrIo[Vector[(ModelParameterIdentifier, Int)]] =
    ResultOrErrIo.fromCalculation(
      domainDimensions.map(i => (ModelParameterIdentifier(i._1), i._2)).toVector
    )

  override private[thylacine] val getValidated = this

  override val domainDimension: Int = domainDimensions.values.sum

  private[thylacine] override def evalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer] =
    ResultOrErrIo.fromCalculation {
      VectorContainer(
        evaluation(
          input.index.map(i => i._1.value -> i._2.rawVector.toScalaVector)
        )
      )
    }
}
