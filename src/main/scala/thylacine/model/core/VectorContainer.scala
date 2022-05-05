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

import breeze.linalg._

case class VectorContainer(
    values: Map[Int, Double],
    dimension: Int,
    validated: Boolean = false,
    parameterLabel: Option[String] = None
) extends Container
    with CanValidate[VectorContainer] {
  if (!validated) {
    assert(values.keys.max <= dimension)
    assert(values.keys.min >= 1)
  }

  override val getValidated: VectorContainer =
    if (validated) this else this.copy(validated = true)

  // Low-level API
  lazy val rawVector: DenseVector[Double] = {
    val vecResult =
      DenseVector.zeros[Double](dimension)
    values.foreach { i =>
      vecResult.update(i._1 - 1, i._2)
    }
    vecResult
  }

  lazy val valueSum: Double = values.values.sum

  // Low-level API
  // Disjoint concatenation of vectors
  def rawConcatenateWith(input: VectorContainer): VectorContainer =
    VectorContainer(
      values ++ input.getValidated.values.map(i => i._1 + dimension -> i._2),
      dimension = dimension + input.dimension,
      validated = true
    )

  // Low-level API
  // Sum of vectors. Dimension check needs to be done outside
  // of this call
  def rawSumWith(input: VectorContainer): VectorContainer =
    VectorContainer(
      values = (values.keySet ++ input.values.keySet).map { k =>
        k -> (values.getOrElse(k, 0d) + input.values.getOrElse(k, 0d))
      }.toMap,
      dimension = dimension,
      validated = true
    )

  def rawScalarProductWith(input: Double): VectorContainer =
    VectorContainer(
      values = values.view.mapValues(_ * input).toMap,
      dimension = dimension,
      validated = true
    )

  def rawSubtract(input: VectorContainer): VectorContainer =
    rawSumWith(input.rawScalarProductWith(-1.0))

  def rawProductWith(input: VectorContainer): VectorContainer =
    VectorContainer(
      values = (values.keySet ++ input.values.keySet).map { k =>
        k -> (values.getOrElse(k, 0d) * input.values.getOrElse(k, 0d))
      }.toMap,
      dimension = dimension,
      validated = true
    )

  def rawDotProductWith(input: VectorContainer): Double =
    rawProductWith(input).valueSum

  lazy val rawAbsoluteValueOfComponents: VectorContainer =
    this.copy(values = values.map(i => i._1 -> Math.abs(i._2)))

}

object VectorContainer {

  def apply(vector: DenseVector[Double]): VectorContainer =
    VectorContainer(vector.toScalaVector().toList)

  def apply(vector: List[Double]): VectorContainer = {
    val resultMap = vector.foldLeft((1, Map[Int, Double]())) { (i, j) =>
      if (j != 0) {
        (i._1 + 1, i._2 + (i._1 -> j))
      } else {
        (i._1 + 1, i._2)
      }
    }

    VectorContainer(
      resultMap._2,
      vector.length,
      validated = true
    )
  }

  def random(dimension: Int): VectorContainer =
    VectorContainer((1 to dimension).map(_ => Math.random()).toList)
}
