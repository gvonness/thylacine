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
package thylacine.model.optimization

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection

import cats.syntax.all._

import scala.annotation.unused

trait ModelParameterOptimizer[F[_]] {
  this: AsyncImplicits[F] =>

  protected def calculateMaximumLogPdf(
    startingPt: ModelParameterCollection
  ): F[(Double, ModelParameterCollection)]

  final def findMaximumLogPdf(startPt: Map[String, Vector[Double]]): F[(Double, Map[String, Vector[Double]])] =
    calculateMaximumLogPdf(IndexedVectorCollection(startPt)).map { case (bestLogPdf, argMax) =>
      (bestLogPdf, argMax.genericScalaRepresentation)
    }

  @unused
  final def findMaximumPdf(startPt: Map[String, Vector[Double]]): F[(Double, Map[String, Vector[Double]])] =
    findMaximumLogPdf(startPt).map(i => (Math.exp(i._1), i._2))
}
