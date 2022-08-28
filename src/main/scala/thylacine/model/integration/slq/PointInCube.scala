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
import thylacine.model.core.computation.Erratum.UnexpectedErratum
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.VectorContainer

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] case class PointInCube[F[_]: Async](
    pointInIntervals: Vector[PointInInterval[F]],
    validated: Boolean = false
) extends CanValidate[PointInCube[F]] {

  private[thylacine] val dimension: Int = pointInIntervals.size

  private[thylacine] lazy val cubeVolume: ResultOrErrF[F, BigDecimal] =
    pointInIntervals.parTraverse {
      _.intervalLength.map(l => BigDecimal(l.toString))
    }.map(_.sum)

  private[thylacine] lazy val symmetrize: ResultOrErrF[F, PointInCube[F]] =
    for {
      symmetrizedIntervals <- pointInIntervals.parTraverse(_.symmetrize)
    } yield PointInCube(symmetrizedIntervals, validated = true)

  private[thylacine] lazy val point: VectorContainer =
    VectorContainer(pointInIntervals.map(_.point))

  private[thylacine] override lazy val getValidated: PointInCube[F] =
    PointInCube(pointInIntervals.map(_.getValidated), validated = true)

  private lazy val dimensionIndex: ResultOrErrF[F, Map[Int, PointInInterval[F]]] =
    (1 to pointInIntervals.size).zip(pointInIntervals).toMap.toResultM

  private[thylacine] def retrieveIndex(index: Int): ResultOrErrF[F, PointInInterval[F]] =
    for {
      dimIndex <- dimensionIndex
      result <- dimIndex.get(index) match {
                  case Some(res) => res.toResultM
                  case _ =>
                    UnexpectedErratum(
                      s"Unable to find index for retrieval in PointInCube: $index"
                    ).toResultM
                }
    } yield result

  private[thylacine] def replaceIndex(
      index: Int,
      newInput: PointInInterval[F]
  ): ResultOrErrF[F, PointInCube[F]] =
    if (index > 0 && index <= dimension) {
      val (foreList, aftList) = pointInIntervals.splitAt(index - 1)
      this.copy(pointInIntervals = foreList ++ List(newInput.getValidated) ++ aftList.tail).toResultM
    } else {
      UnexpectedErratum(
        s"Index out of bounds for replacement in PointInCube: $index"
      ).toResultM
    }

  private[thylacine] def getSample(scalingParameter: Double): ResultOrErrF[F, VectorContainer] =
    for {
      dimensionSamples <-
        pointInIntervals.parTraverse(_.getSample(scalingParameter))
      result <-
        VectorContainer(dimensionSamples).toResultM
    } yield result

  def isIntersectingWith(input: PointInCube[F]): ResultOrErrF[F, Boolean] =
    pointInIntervals
      .zip(input.pointInIntervals)
      .parTraverse(i => i._1.isIntersectingWith(i._2).map(!_))
      .map(_.exists(i => i))

  private[thylacine] def dimensionOfLargestSeparation(input: PointInCube[F]): ResultOrErrF[F, Int] =
    for {
      distances <- pointInIntervals
                     .zip(input.pointInIntervals)
                     .parTraverse(i => i._1.distanceSquaredFrom(i._2))
    } yield (1 to pointInIntervals.size).zip(distances).maxBy(_._2)._1
}

private[thylacine] object PointInCube {

  private[thylacine] def makeDisjoint[F[_]: Async](
      pic1: PointInCube[F],
      pic2: PointInCube[F]
  ): ResultOrErrF[F, (PointInCube[F], PointInCube[F])] =
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
                  (pic1, pic2).toResultM
                }
    } yield result

  private[thylacine] def makeDisjoint[F[_]: Async](
      cubes: Vector[PointInCube[F]]
  ): ResultOrErrF[F, Vector[PointInCube[F]]] = {
    def makeNewCubeDisjoint(
        newCube: PointInCube[F],
        disjointCubes: Vector[PointInCube[F]]
    ): ResultOrErrF[F, Vector[PointInCube[F]]] =
      disjointCubes.foldLeft(Vector(newCube).toResultM) { (i, j) =>
        for {
          prev   <- i
          result <- PointInCube.makeDisjoint(prev.head, j)
        } yield Vector(result._1, result._2) ++ prev.tail
      }

    cubes.foldLeft(Vector[PointInCube[F]]().toResultM) { (i, j) =>
      for {
        prev   <- i
        result <- makeNewCubeDisjoint(j, prev)
      } yield result
    }
  }
}
