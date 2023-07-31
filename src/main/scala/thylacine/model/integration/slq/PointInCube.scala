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
package thylacine.model.integration.slq

import thylacine.model.core._
import thylacine.model.core.values.VectorContainer

private[thylacine] case class PointInCube(
  pointInIntervals: Vector[PointInInterval],
  validated: Boolean = false
) extends CanValidate[PointInCube] {

  private[thylacine] val dimension: Int = pointInIntervals.size

  private[thylacine] lazy val cubeVolume: BigDecimal =
    pointInIntervals.map { pointInterval =>
      BigDecimal(pointInterval.intervalLength.toString)
    }.sum

  private[thylacine] lazy val symmetrize: PointInCube =
    PointInCube(pointInIntervals.map(_.symmetrize), validated = true)

  private[thylacine] lazy val point: VectorContainer =
    VectorContainer(pointInIntervals.map(_.point))

  private[thylacine] override lazy val getValidated: PointInCube =
    PointInCube(pointInIntervals.map(_.getValidated), validated = true)

  private lazy val dimensionIndex: Map[Int, PointInInterval] =
    (1 to pointInIntervals.size).zip(pointInIntervals).toMap

  private[thylacine] def retrieveIndex(index: Int): PointInInterval =
    dimensionIndex(index)

  private[thylacine] def replaceIndex(
    index: Int,
    newInput: PointInInterval
  ): PointInCube =
    if (index > 0 && index <= dimension) {
      val (foreList, aftList) = pointInIntervals.splitAt(index - 1)
      this.copy(pointInIntervals = foreList ++ List(newInput.getValidated) ++ aftList.tail)
    } else {
      throw new RuntimeException(
        s"Index out of bounds for replacement in PointInCube: $index"
      )
    }

  private[thylacine] def getSample(scalingParameter: Double): VectorContainer =
    VectorContainer(pointInIntervals.map(_.getSample(scalingParameter)))

  def isIntersectingWith(input: PointInCube): Boolean =
    pointInIntervals
      .zip(input.pointInIntervals)
      .forall(i => i._1.isIntersectingWith(i._2))

  private[thylacine] def dimensionOfLargestSeparation(input: PointInCube): Int =
    (1 to pointInIntervals.size)
      .zip {
        pointInIntervals
          .zip(input.pointInIntervals)
          .map(i => i._1.distanceSquaredFrom(i._2))
      }
      .maxBy(_._2)
      ._1
}

private[thylacine] object PointInCube {

  private[thylacine] def makeDisjoint(
    pic1: PointInCube,
    pic2: PointInCube
  ): (PointInCube, PointInCube) =
    if (pic1.isIntersectingWith(pic2)) {
      val dimensionOfLargestSeparation = pic1.dimensionOfLargestSeparation(pic2)
      val (newInterval1, newInterval2) = PointInInterval.findDisjointBoundary(
        pic1.retrieveIndex(dimensionOfLargestSeparation),
        pic2.retrieveIndex(dimensionOfLargestSeparation)
      )
      val newPic1 = pic1.replaceIndex(dimensionOfLargestSeparation, newInterval1)
      val newPic2 = pic2.replaceIndex(dimensionOfLargestSeparation, newInterval2)

      (newPic1, newPic2)
    } else {
      (pic1, pic2)
    }

  private[thylacine] def makeDisjoint(
    cubes: Vector[PointInCube]
  ): Vector[PointInCube] = {
    def makeNewCubeDisjoint(
      newCube: PointInCube,
      disjointCubes: Vector[PointInCube]
    ): Vector[PointInCube] =
      disjointCubes.foldLeft(Vector(newCube)) { case (cubeAccumulation, currentCube) =>
        val (disjointCube1, disjointCube2) = PointInCube.makeDisjoint(cubeAccumulation.head, currentCube)
        Vector(disjointCube1, disjointCube2) ++ cubeAccumulation.tail
      }

    cubes.foldLeft(Vector[PointInCube]()) { case (pointInCubeAccumulation, currentCube) =>
      makeNewCubeDisjoint(currentCube, pointInCubeAccumulation)
    }
  }
}
