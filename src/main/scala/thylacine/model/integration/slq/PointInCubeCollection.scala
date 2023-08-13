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
package thylacine.model.integration.slq

import thylacine.model.core.*
import thylacine.model.core.values.VectorContainer
import thylacine.util.MathOps

private[thylacine] case class PointInCubeCollection(
  pointsInCube: Vector[PointInCube],
  validated: Boolean = false
) extends CanValidate[PointInCubeCollection] {
  if (!validated) {
    assert(pointsInCube.size > 1)
  }

  private[thylacine] val dimension: Int =
    pointsInCube.headOption.map(_.dimension).getOrElse(0)

  if (!validated) {
    assert(
      validated || pointsInCube.size == pointsInCube.map(_.point).toSet.size
    )
    assert(validated || pointsInCube.tail.forall(_.dimension == dimension))
  }

  private[thylacine] lazy val pointsOnly: Vector[VectorContainer] =
    pointsInCube.map(_.point)

  override private[thylacine] lazy val getValidated: PointInCubeCollection =
    if (validated) {
      this
    } else {
      PointInCubeCollection(
        pointsInCube = pointsInCube.map(_.getValidated),
        validated    = true
      )
    }

  // Initialises cube bounds to symmetrically reflect the greatest
  // distance between the point in question and any other point in
  // the collection along the dimension in question. This ensures
  // that the valid solution space around the point collection is
  // well covered.
  private lazy val initialise: PointInCubeCollection = {
    val picList = pointsInCube.map { pic =>
      val absoluteDifferences = pointsOnly.map(_.rawSubtract(pic.point).rawAbsoluteValueOfComponents)
      val piiList = (1 to dimension)
        .zip(pic.point.scalaVector.toList)
        .toVector
        .map { k =>
          val maxDiff = absoluteDifferences
            .maxBy(
              _.values.getOrElse(k._1, 0d)
            )
            .values
            .getOrElse(k._1, 0d)
          PointInInterval(
            point      = k._2,
            lowerBound = k._2 - maxDiff,
            upperBound = k._2 + maxDiff,
            validated  = true
          )
        }
      PointInCube(piiList, validated = true)
    }

    PointInCubeCollection(picList, validated = true)
  }

  private lazy val makeDisjoint: PointInCubeCollection =
    PointInCubeCollection(PointInCube.makeDisjoint(pointsInCube), validated = true)

  private lazy val symmetrize: PointInCubeCollection =
    PointInCubeCollection(pointsInCube.map(_.symmetrize), validated = true)

  private[thylacine] lazy val readyForSampling: PointInCubeCollection =
    getValidated.initialise.makeDisjoint.symmetrize

  private lazy val sampleMapping: Vector[((BigDecimal, BigDecimal), PointInCube)] =
    MathOps.cdfStaircase(pointsInCube.map(_.cubeVolume)).zip(pointsInCube)

  private[thylacine] def getSample(
    scaleParameter: Double
  ): VectorContainer = {
    val randomIndex = BigDecimal(Math.random().toString)

    sampleMapping
      .collect {
        case ((stairCaseLower, stairCaseUpper), cube)
            if randomIndex >= stairCaseLower && randomIndex < stairCaseUpper =>
          cube
      }
      .head
      .getSample(scaleParameter)
  }
}

private[thylacine] object PointInCubeCollection {

  private[thylacine] val empty: PointInCubeCollection =
    PointInCubeCollection(Vector(), validated = true)
}
