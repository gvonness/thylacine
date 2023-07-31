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
package thylacine.model.core.values

import thylacine.model.core.CanValidate
import thylacine.util.MathOps

import breeze.linalg._

import scala.{ Vector => ScalaVector }

private[thylacine] case class VectorContainer(
  values: Map[Int, Double],
  dimension: Int,
  validated: Boolean             = false,
  parameterLabel: Option[String] = None
) extends Container
    with CanValidate[VectorContainer] {
  if (!validated) {
    assert(values.isEmpty || values.keys.max <= dimension)
    assert(values.isEmpty || values.keys.min >= 1)
  }

  private[thylacine] override val getValidated: VectorContainer =
    if (validated) this else this.copy(values = values.filter(_._2 != 0d), validated = true)

  // Low-level API
  private[thylacine] lazy val rawVector: DenseVector[Double] = {
    val vecResult =
      DenseVector.zeros[Double](dimension)
    values.foreach { i =>
      vecResult.update(i._1 - 1, i._2)
    }
    vecResult
  }

  private[thylacine] lazy val squaredMagnitude: Double =
    values.values.map(Math.pow(_, 2)).sum

  private[thylacine] lazy val magnitude: Double =
    Math.sqrt(squaredMagnitude)

  private[thylacine] lazy val scalaVector: ScalaVector[Double] =
    rawVector.toScalaVector

  private[thylacine] lazy val valueSum: Double = values.values.sum

  // Low-level API
  // Disjoint concatenation of vectors
  private[thylacine] def rawConcatenateWith(
    input: VectorContainer
  ): VectorContainer =
    VectorContainer(
      values ++ input.getValidated.values.map(i => i._1 + dimension -> i._2),
      dimension = dimension + input.dimension
    ).getValidated

  // Low-level API
  // Sum of vectors. Dimension check needs to be done outside
  // of this call
  private[thylacine] def rawSumWith(input: VectorContainer): VectorContainer =
    VectorContainer(
      values = (values.keySet ++ input.values.keySet).map { k =>
        k -> (values.getOrElse(k, 0d) + input.values.getOrElse(k, 0d))
      }.toMap,
      dimension = dimension
    ).getValidated

  private[thylacine] def rawScalarProductWith(input: Double): VectorContainer =
    VectorContainer(
      values    = values.view.mapValues(_ * input).toMap,
      dimension = dimension
    ).getValidated

  private[thylacine] def rawSubtract(input: VectorContainer): VectorContainer =
    rawSumWith(input.rawScalarProductWith(-1.0))

  private[thylacine] def rawProductWith(
    input: VectorContainer
  ): VectorContainer =
    VectorContainer(
      values = (values.keySet ++ input.values.keySet).map { k =>
        k -> (values.getOrElse(k, 0d) * input.values.getOrElse(k, 0d))
      }.toMap,
      dimension = dimension
    ).getValidated

  private[thylacine] def rawDotProductWith(input: VectorContainer): Double =
    rawProductWith(input).valueSum

  private[thylacine] lazy val rawAbsoluteValueOfComponents: VectorContainer =
    this.copy(values = values.map(i => i._1 -> Math.abs(i._2)))

  // Minimal checks for performance, as this is only intended to be used in
  // finite differences for gradient calculation
  private def rawNudgeComponent(diff: Double, index: Int): VectorContainer =
    this.copy(values = values + (index -> (values.getOrElse(index, 0d) + diff))).getValidated

  private[thylacine] def rawNudgeComponents(
    diff: Double
  ): List[VectorContainer] =
    (1 to dimension).map(rawNudgeComponent(diff, _)).toList

}

private[thylacine] object VectorContainer {

  private[thylacine] def apply(vector: ScalaVector[Double]): VectorContainer = {
    val resultMap = vector.foldLeft((1, Map[Int, Double]())) { (i, j) =>
      if (j != 0) {
        (i._1 + 1, i._2 + (i._1 -> j))
      } else {
        (i._1 + 1, i._2)
      }
    }

    VectorContainer(
      resultMap._2,
      vector.length
    ).getValidated
  }

  private[thylacine] def apply(vector: DenseVector[Double]): VectorContainer =
    VectorContainer(vector.toArray.toVector)

  private[thylacine] def fill(dimension: Int)(value: Double): VectorContainer =
    VectorContainer((1 to dimension).map(_ => value).toVector)

  private[thylacine] def zeros(dimension: Int): VectorContainer =
    fill(dimension)(0d)

  private[thylacine] def random(dimension: Int): VectorContainer =
    VectorContainer((1 to dimension).map(_ => MathOps.nextGaussian).toVector)
}
