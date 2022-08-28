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
import thylacine.util.MathOps

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

private[thylacine] case class PointInCubeCollection[F[_]: Async](
    pointsInCube: Vector[PointInCube[F]],
    validated: Boolean = false
) extends CanValidate[PointInCubeCollection[F]] {
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

  private[thylacine] override lazy val getValidated: PointInCubeCollection[F] =
    if (validated) {
      this
    } else {
      PointInCubeCollection(
        pointsInCube = pointsInCube.map(_.getValidated),
        validated = true
      )
    }

  // Initialises cube bounds to symmetrically reflect the greatest
  // distance between the point in question and any other point in
  // the collection along the dimension in question. This ensures
  // that the valid solution space around the point collection is
  // well covered.
  private lazy val initialise: ResultOrErrF[F, PointInCubeCollection] =
    for {
      picList <-
        pointsInCube.traverse { // Do not do this in parallel as it will exhaust VM memory
          pic =>
            for {
              absoluteDifferences <-
                pointsOnly.parTraverse(_.rawSubtract(pic.point).rawAbsoluteValueOfComponents.toResultM)
              piiList <-
                (1 to dimension)
                  .zip(pic.point.scalaVector.toList)
                  .toVector
                  .parTraverse { k =>
                    for {
                      maxDiff <- absoluteDifferences
                                   .maxBy(
                                     _.values.getOrElse(k._1, 0d)
                                   )
                                   .values
                                   .getOrElse(k._1, 0d)
                                   .toResultM
                      result <- PointInInterval(
                                  point = k._2,
                                  lowerBound = k._2 - maxDiff,
                                  upperBound = k._2 + maxDiff,
                                  validated = true
                                ).toResultM
                    } yield result
                  }
              newPic <- PointInCube(piiList, validated = true).toResultM
            } yield newPic
        }
      newPicCollection <- PointInCubeCollection(picList, validated = true).toResultM
    } yield newPicCollection

  private lazy val makeDisjoint: ResultOrErrF[F, PointInCubeCollection[F]] =
    for {
      result <- PointInCube.makeDisjoint(pointsInCube)
    } yield PointInCubeCollection(result, validated = true)

  private lazy val symmetrize: ResultOrErrF[F, PointInCubeCollection[F]] =
    for {
      symmetrizedCubes <- pointsInCube.parTraverse(_.symmetrize)
    } yield PointInCubeCollection(symmetrizedCubes, validated = true)

  private[thylacine] lazy val readyForSampling: ResultOrErrF[F, PointInCubeCollection[F]] =
    for {
      init            <- getValidated.initialise
      initDisjoint    <- init.makeDisjoint
      initDisjointSym <- initDisjoint.symmetrize
    } yield initDisjointSym

  private lazy val sampleMapping: ResultOrErrF[F, Vector[((BigDecimal, BigDecimal), PointInCube[F])]] = {
    for {
      cubeVolumes  <- pointsInCube.parTraverse(_.cubeVolume)
      cdfStaircase <- MathOps.cdfStaircase(cubeVolumes)
    } yield cdfStaircase.zip(pointsInCube)
  }

  private[thylacine] def getSample(
      scaleParameter: Double
  ): ResultOrErrF[F, VectorContainer] =
    for {
      randomIndex  <- BigDecimal(Math.random().toString).toResultM
      rawSampleMap <- sampleMapping
      selectedCubes <- rawSampleMap.parTraverse { sm =>
                         (if (randomIndex >= sm._1._1 && randomIndex < sm._1._2)
                            Some(sm._2)
                          else None).toResultM
                       }.map(_.flatten)
      result <- selectedCubes.headOption match {
                  case Some(head) => head.getSample(scaleParameter)
                  case _ =>
                    UnexpectedErratum(
                      s"Failed to resolve volume index for cube collection sampling: $randomIndex"
                    ).toResultM
                }
    } yield result
}

private[thylacine] object PointInCubeCollection {

  private[thylacine] def empty[F[_]: Async]: PointInCubeCollection[F] =
    PointInCubeCollection(Vector(), validated = true)
}
