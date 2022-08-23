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

import scala.annotation.tailrec

// Mutable structure, as our quadratures may contain a large number
// of points. This should not be accessed outside of a transactional
// variable
private[thylacine] case class QuadratureAbscissa(
    samplePool: Set[Double],
    abscissa: Vector[Double]
) {
  import QuadratureAbscissa._

  private[thylacine] lazy val maxSample: Double = samplePool.max

  private[thylacine] lazy val extendAbscissaByOne: QuadratureAbscissa = {
    val newSample: Double = getNext(maxSample, existing = samplePool)

    QuadratureAbscissa((samplePool - maxSample) + newSample, maxSample +: abscissa)
  }

  private[thylacine] lazy val getTrapezoidalQuadrature: Vector[Double] = {
    val differences: Vector[Double] =
      -abscissa.take(2).reduce(_ - _) +:
        abscissa
          .drop(2)
          .zip(abscissa.dropRight(2))
          .map(i => i._1 - i._2) :+
        -abscissa.takeRight(2).reduce(_ - _)

    differences.map(i => 0.5 * i)
  }
}

private[thylacine] object QuadratureAbscissa {

  @tailrec
  private def getNext(max: Double, existing: Set[Double]): Double = {
    val candidate: Double = Math.random() * max
    if (!existing.contains(candidate)) {
      candidate
    } else {
      getNext(max, existing)
    }
  }

  private[thylacine] def apply(numSamples: Int): QuadratureAbscissa = {
    val randomVector = (1 until numSamples).foldLeft(Set[Double](1.0)) { (i, _) =>
      i + getNext(max = 1.0, existing = i)
    }

    QuadratureAbscissa(
      samplePool = randomVector,
      abscissa = Vector()
    )
  }
}
