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
package thylacine.model.core

case class BelievedData(
    data: VectorContainer,
    covariance: MatrixContainer,
    validated: Boolean = false
) extends CanValidate[BelievedData] {
  if (!validated) {
    assert(covariance.rowTotalNumber == covariance.columnTotalNumber)
    assert(data.dimension == covariance.rowTotalNumber)
  }

  val dimension: Int = data.dimension

  override lazy val getValidated: BelievedData =
    if (validated) {
      this
    } else {
      BelievedData(
        data.getValidated,
        covariance.getValidated,
        validated = true
      )
    }
}

object BelievedData {

  def apply(
      values: VectorContainer,
      symmetricConfidenceIntervals: VectorContainer
  ): BelievedData = {
    val validatedValues: VectorContainer = values.getValidated
    val validatedConfidenceIntervals: VectorContainer =
      symmetricConfidenceIntervals.getValidated

    assert(validatedValues.dimension == validatedConfidenceIntervals.dimension)
    assert(validatedConfidenceIntervals.values.values.forall(_ > 0))

    BelievedData(
      data = validatedValues,
      covariance = MatrixContainer(
        values = validatedConfidenceIntervals.values.map(i =>
          (i._1, i._1) -> Math.pow(i._2 / 2, 2)
        ),
        rowTotalNumber = values.dimension,
        columnTotalNumber = values.dimension,
        validated = true
      ),
      validated = true
    )
  }
}
