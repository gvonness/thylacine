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
import thylacine.util.MathOps

import ai.entrolution.thylacine.model.core.values.VectorContainer
import cats.implicits._

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

  private[thylacine] override lazy val getValidated: PointInCubeCollection =
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
  private lazy val initialise: ResultOrErrIo[PointInCubeCollection] =
    for {
      picList <-
        pointsInCube.traverse { // Do not do this in parallel as it will exhaust VM memory
          pic =>
            for {
              absoluteDifferences <-
                pointsOnly.parTraverse { i =>
                  ResultOrErrIo.fromCalculation {
                    i.rawSubtract(pic.point).rawAbsoluteValueOfComponents
                  }
                }
              piiList <-
                (1 to dimension)
                  .zip(pic.point.scalaVector.toList)
                  .toVector
                  .parTraverse { k =>
                    for {
                      maxDiff <- ResultOrErrIo.fromCalculation {
                                   absoluteDifferences
                                     .maxBy(
                                       _.values.getOrElse(k._1, 0d)
                                     )
                                     .values
                                     .getOrElse(k._1, 0d)
                                 }
                      result <-
                        ResultOrErrIo.fromCalculation {
                          PointInInterval(
                            point = k._2,
                            lowerBound = k._2 - maxDiff,
                            upperBound = k._2 + maxDiff,
                            validated = true
                          )
                        }
                    } yield result
                  }
              newPic <- ResultOrErrIo.fromCalculation(
                          PointInCube(piiList, validated = true)
                        )
            } yield newPic
        }
      newPicCollection <- ResultOrErrIo.fromCalculation(
                            PointInCubeCollection(picList, validated = true)
                          )
    } yield newPicCollection

  private lazy val makeDisjoint: ResultOrErrIo[PointInCubeCollection] =
    for {
      result <- PointInCube.makeDisjoint(pointsInCube)
    } yield PointInCubeCollection(result, validated = true)

  private lazy val symmetrize: ResultOrErrIo[PointInCubeCollection] =
    for {
      symmetrizedCubes <- pointsInCube.traverse(_.symmetrize)
    } yield PointInCubeCollection(symmetrizedCubes, validated = true)

  private[thylacine] lazy val readyForSampling: ResultOrErrIo[PointInCubeCollection] =
    for {
      init            <- getValidated.initialise
      initDisjoint    <- init.makeDisjoint
      initDisjointSym <- initDisjoint.symmetrize
    } yield initDisjointSym

  private lazy val sampleMapping: ResultOrErrIo[Vector[((BigDecimal, BigDecimal), PointInCube)]] = {
    for {
      cubeVolumes  <- pointsInCube.parTraverse(_.cubeVolume)
      cdfStaircase <- MathOps.cdfStaircase(cubeVolumes)
    } yield cdfStaircase.zip(pointsInCube)
  }

  private[thylacine] def getSample(
      scaleParameter: Double
  ): ResultOrErrIo[VectorContainer] =
    for {
      randomIndex <-
        ResultOrErrIo.fromCalculation(BigDecimal(Math.random().toString))
      rawSampleMap <- sampleMapping
      selectedCubes <- rawSampleMap.parTraverse { sm =>
                         ResultOrErrIo.fromCalculation {
                           if (randomIndex >= sm._1._1 && randomIndex < sm._1._2)
                             Some(sm._2)
                           else None
                         }
                       }.map(_.flatten)
      result <- selectedCubes.headOption match {
                  case Some(head) => head.getSample(scaleParameter)
                  case _ =>
                    ResultOrErrIo.fromErratum(
                      UnexpectedErratum(
                        s"Failed to resolve volume index for cube collection sampling: $randomIndex"
                      )
                    )
                }
    } yield result
}

private[thylacine] object PointInCubeCollection {

  private[thylacine] val empty: PointInCubeCollection =
    PointInCubeCollection(Vector(), validated = true)
}
