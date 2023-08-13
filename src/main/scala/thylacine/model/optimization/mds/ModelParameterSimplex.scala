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
package thylacine.model.optimization.mds

import thylacine.model.core.CanValidate
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.modelparameters.ModelParameterContext
import thylacine.model.optimization.mds.ModelParameterSimplex.differenceMagnitude
import thylacine.util.MathOps

private[thylacine] case class ModelParameterSimplex(
  private[thylacine] val vertices: Map[Int, Vector[Double]],
  private[thylacine] val isRegular: Boolean = false,
  private[thylacine] val validated: Boolean = false
) extends CanValidate[ModelParameterSimplex] {

  private lazy val randomAdjacentEdgeLength: Double =
    Math.sqrt(vertices.take(2).values.reduce(_.zip(_).map(i => Math.pow(i._1 - i._2, 2.0))).sum)

  if (!validated) {
    val rawVertices = vertices.values.toList
    val distancesAllEqual: Boolean =
      rawVertices.init
        .foldLeft((true, rawVertices.tail)) {
          case ((allEqualSoFar, remainingVertices), vertex) if allEqualSoFar =>
            if (
              remainingVertices
                .map(differenceMagnitude(_, vertex))
                .forall(magnitude => Math.abs(magnitude - randomAdjacentEdgeLength) < .001)
            ) {
              (true, remainingVertices.tail)
            } else {
              (false, List())
            }
          case (notAllEqualSoFar, _) => notAllEqualSoFar
        }
        ._1

    assert(!(isRegular ^ distancesAllEqual))
  }

  override private[thylacine] lazy val getValidated: ModelParameterSimplex =
    this.copy(validated = true)

  private[thylacine] def verticesAsModelParameters(
    modelParameterContext: ModelParameterContext
  ): Map[Int, ModelParameterCollection] =
    vertices.view.mapValues(modelParameterContext.vectorValuesToModelParameterCollection).toMap

  private[thylacine] def reflectAbout(vertexIndex: Int): ModelParameterSimplex = {
    val doubleReflectionVertex = vertices(vertexIndex).map(_ * 2.0)
    this.copy(vertices = vertices.view.mapValues(doubleReflectionVertex.zip(_).map(i => i._1 - i._2)).toMap)
  }

  private[thylacine] def expandAbout(vertexIndex: Int, mu: Double): ModelParameterSimplex = {
    val baseVertex = vertices(vertexIndex).map(_ * (1 - mu))
    this.copy(vertices = vertices.view.mapValues(v => baseVertex.zip(v.map(_ * mu)).map(i => i._1 + i._2)).toMap)
  }

  private[thylacine] def contractAbout(vertexIndex: Int, theta: Double): ModelParameterSimplex = {
    val baseVertex = vertices(vertexIndex).map(_ * (1 + theta))
    this.copy(vertices = vertices.view.mapValues(v => baseVertex.zip(v.map(_ * theta)).map(i => i._1 - i._2)).toMap)
  }

  private[thylacine] def maxAdjacentEdgeLength(vertexIndex: Int): Double =
    if (isRegular) {
      randomAdjacentEdgeLength
    } else {
      val vertex = vertices(vertexIndex)

      (vertices - vertexIndex).values.map(differenceMagnitude(_, vertex)).max
    }
}

private[thylacine] object ModelParameterSimplex {

  private def differenceMagnitude(input1: Vector[Double], input2: Vector[Double]): Double =
    Math.sqrt(input1.zip(input2).map(i => Math.pow(i._1 - i._2, 2.0)).sum)

  private[thylacine] def unitRegularCenteredOn(
    input: ModelParameterCollection,
    rawMappings: ModelParameterContext
  ): ModelParameterSimplex = {
    val n                = input.totalDimension
    val baseScalar       = -Math.pow(n.toDouble, -1.5) * (Math.sqrt(n + 1d) + 1)
    val nudgeScalar      = Math.sqrt(1d + (1d / n))
    val lastVertexScalar = Math.pow(n.toDouble, -.5)

    val vectorInput: Vector[Double] = rawMappings.modelParameterCollectionToVectorValues(input)

    val lastVertex: Vector[Double] = Vector.fill(vectorInput.size)(lastVertexScalar)

    val baseVertex: Vector[Double] = Vector.fill(vectorInput.size)(baseScalar)

    val recenteredVertices =
      (baseVertex.indices
        .map(index => MathOps.modifyVectorIndex(baseVertex)(index, _ + nudgeScalar)) :+ lastVertex)
        .map(_.zip(vectorInput).map(i => i._1 + i._2))

    ModelParameterSimplex(
      recenteredVertices.indices
        .zip(recenteredVertices)
        .toMap,
      isRegular = true
    ).getValidated
  }

}
