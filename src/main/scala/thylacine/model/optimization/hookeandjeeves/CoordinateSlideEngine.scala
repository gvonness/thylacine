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
package thylacine.model.optimization.hookeandjeeves

import thylacine.model.components.posterior.Posterior
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.optimization.line.GoldenSectionSearch

import cats.effect.kernel.Async
import cats.syntax.all._

import scala.util.Random
import scala.{Vector => ScalaVector}

// Modification of standard Hooke and Jeeves to leverage
// line searches along each coordinate direction.
// Under a particular direction, the line search either refines the
// initial triad between the endpoints or explores in the direction
// of greatest increase
private[thylacine] trait CoordinateSlideEngine[F[_]] extends HookeAndJeevesEngine[F] with GoldenSectionSearch[F] {
  this: StmImplicits[F] with Posterior[F, Prior[F, _], _] =>

  protected override val telemetryPrefix: String = "Coordinate Slide"

  protected override def dimensionScan(
      nudgeAmount: Double,
      startingPoint: ScalaVector[Double],
      startingLogPdf: Double
  ): F[(Double, ScalaVector[Double])] =
    Random
      .shuffle(startingPoint.indices.toList)
      .foldLeft(Async[F].pure((startingLogPdf, startingPoint))) { case (previousF, testIndex) =>
        previousF.flatMap { case previous @ (_, currentArgMax) =>
          List(-nudgeAmount, nudgeAmount)
            .traverse(nudgeAndEvaluate(testIndex, _, currentArgMax))
            .flatMap { results =>
              searchColinearTriple(results.head, previous, results.last)
            }
        }
      }

}
