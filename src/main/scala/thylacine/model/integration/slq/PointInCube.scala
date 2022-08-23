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

import thylacine.model.core.Erratum.{ResultOrErrIo, _}
import thylacine.model.core._

import cats.implicits._

private[thylacine] case class PointInCube(
    pointInIntervals: Vector[PointInInterval],
    validated: Boolean = false
) extends CanValidate[PointInCube] {

  private[thylacine] val dimension: Int = pointInIntervals.size

  private[thylacine] lazy val cubeVolume: ResultOrErrIo[BigDecimal] =
    pointInIntervals.parTraverse {
      _.intervalLength.map(l => BigDecimal(l.toString))
    }.map(_.sum)

  private[thylacine] lazy val symmetrize: ResultOrErrIo[PointInCube] =
    for {
      symmetrizedIntervals <- pointInIntervals.parTraverse(_.symmetrize)
    } yield PointInCube(symmetrizedIntervals, validated = true)

  private[thylacine] lazy val point: VectorContainer =
    VectorContainer(pointInIntervals.map(_.point))

  private[thylacine] override lazy val getValidated: PointInCube =
    PointInCube(pointInIntervals.map(_.getValidated), validated = true)

  private lazy val dimensionIndex: ResultOrErrIo[Map[Int, PointInInterval]] =
    ResultOrErrIo.fromCalculation(
      (1 to pointInIntervals.size).zip(pointInIntervals).toMap
    )

  private[thylacine] def retrieveIndex(index: Int): ResultOrErrIo[PointInInterval] =
    for {
      dimIndex <- dimensionIndex
      result <- dimIndex.get(index) match {
                  case Some(res) => ResultOrErrIo.fromValue(res)
                  case _ =>
                    ResultOrErrIo.fromErratum(
                      UnexpectedErratum(
                        s"Unable to find index for retrieval in PointInCube: $index"
                      )
                    )
                }
    } yield result

  private[thylacine] def replaceIndex(
      index: Int,
      newInput: PointInInterval
  ): ResultOrErrIo[PointInCube] =
    if (index > 0 && index <= dimension) {
      val (foreList, aftList) = pointInIntervals.splitAt(index - 1)
      ResultOrErrIo.fromCalculation(
        this.copy(pointInIntervals = foreList ++ List(newInput.getValidated) ++ aftList.tail)
      )
    } else {
      ResultOrErrIo.fromErratum(
        UnexpectedErratum(
          s"Index out of bounds for replacement in PointInCube: $index"
        )
      )
    }

  private[thylacine] def getSample(scalingParameter: Double): ResultOrErrIo[VectorContainer] =
    for {
      dimensionSamples <-
        pointInIntervals.parTraverse(_.getSample(scalingParameter))
      result <-
        ResultOrErrIo.fromCalculation(VectorContainer(dimensionSamples))
    } yield result

  def isIntersectingWith(input: PointInCube): ResultOrErrIo[Boolean] =
    pointInIntervals
      .zip(input.pointInIntervals)
      .parTraverse(i => i._1.isIntersectingWith(i._2).map(!_))
      .map(_.exists(i => i))

  private[thylacine] def dimensionOfLargestSeparation(input: PointInCube): ResultOrErrIo[Int] =
    for {
      distances <- pointInIntervals
                     .zip(input.pointInIntervals)
                     .parTraverse(i => i._1.distanceSquaredFrom(i._2))
    } yield (1 to pointInIntervals.size).zip(distances).maxBy(_._2)._1
}

private[thylacine] object PointInCube {

  private[thylacine] def makeDisjoint(
      pic1: PointInCube,
      pic2: PointInCube
  ): ResultOrErrIo[(PointInCube, PointInCube)] =
    for {
      isIntersecting <- pic1.isIntersectingWith(pic2)
      result <- if (isIntersecting) {
                  for {
                    dimensionOfLargestSeparation <-
                      pic1.dimensionOfLargestSeparation(pic2)
                    interval1 <-
                      pic1.retrieveIndex(dimensionOfLargestSeparation)
                    interval2 <-
                      pic2.retrieveIndex(dimensionOfLargestSeparation)
                    intervalPair <- PointInInterval.findDisjointBoundary(
                                      interval1,
                                      interval2
                                    )
                    (newInterval1, newInterval2) = intervalPair
                    newPic1 <- pic1.replaceIndex(dimensionOfLargestSeparation, newInterval1)
                    newPic2 <- pic2.replaceIndex(dimensionOfLargestSeparation, newInterval2)
                  } yield (newPic1, newPic2)
                } else {
                  ResultOrErrIo.fromValue((pic1, pic2))
                }
    } yield result

  private[thylacine] def makeDisjoint(
      cubes: Vector[PointInCube]
  ): ResultOrErrIo[Vector[PointInCube]] = {
    def makeNewCubeDisjoint(
        newCube: PointInCube,
        disjointCubes: Vector[PointInCube]
    ): ResultOrErrIo[Vector[PointInCube]] =
      disjointCubes.foldLeft(ResultOrErrIo.fromValue(Vector(newCube))) { (i, j) =>
        for {
          prev   <- i
          result <- PointInCube.makeDisjoint(prev.head, j)
        } yield Vector(result._1, result._2) ++ prev.tail
      }

    cubes.foldLeft(ResultOrErrIo.fromValue(Vector[PointInCube]())) { (i, j) =>
      for {
        prev   <- i
        result <- makeNewCubeDisjoint(j, prev)
      } yield result.toVector
    }
  }
}
