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

import thylacine.model.core.Erratum.{ResultOrErrF, ResultOrErrIo}
import thylacine.model.core.IndexedVectorCollection.ModelParameterCollection

import cats.effect.IO

private[thylacine] trait ModelParameterOptimizer {

  protected def calculateMaximumLogPdf(
      startingPt: ModelParameterCollection
  ): ResultOrErrIo[(Double, ModelParameterCollection)]

  final def findMaximumLogPdf(startPt: Map[String, Vector[Double]]): IO[(Double, Map[String, Vector[Double]])] =
    for {
      maximumResult <- calculateMaximumLogPdf(IndexedVectorCollection(startPt)).value
      result <- maximumResult match {
                  case Right(res) =>
                    IO.pure(res)
                  case Left(erratum) =>
                    IO.raiseError(new RuntimeException(erratum.toString))
                }
    } yield (result._1, result._2.genericScalaRepresentation)

  final def findMaximumLogPdf: IO[(Double, Map[String, Vector[Double]])] =
    findMaximumLogPdf(Map())

  final def findMaximumPdf(startPt: Map[String, Vector[Double]]): IO[(Double, Map[String, Vector[Double]])] =
    findMaximumLogPdf(startPt).map(i => (Math.exp(i._1), i._2))

  final def findMaximumPdf: IO[(Double, Map[String, Vector[Double]])] =
    findMaximumPdf(Map())
}
