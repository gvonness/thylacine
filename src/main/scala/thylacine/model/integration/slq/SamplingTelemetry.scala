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

import ch.obermuhlner.math.big.DefaultBigDecimalMath

import scala.collection.parallel.CollectionConverters._

case class SamplingTelemetry(
    logPdfs: List[Double],
    quadratures: List[List[Double]]
) {

  private lazy val minLogPdf: BigDecimal = (logPdfs.max + logPdfs.min) / 2.0

  private lazy val integrandGraphs: List[List[(BigDecimal, BigDecimal)]] =
    quadratures.map(
      _.zip(logPdfs).map(i =>
        (BigDecimal(i._1.toString), BigDecimal((i._2 - minLogPdf).toString))
      )
    )

  private lazy val evidenceStats: List[BigDecimal] =
    integrandGraphs.map { ig =>
      ig.par
        .map(p => BigDecimal(DefaultBigDecimalMath.exp(p._2.bigDecimal)) * p._1)
        .sum
    }

  lazy val negativeEntropyStats: List[Double] =
    try integrandGraphs.map { ig =>
      ig.par
        .map(p =>
          BigDecimal(DefaultBigDecimalMath.exp(p._2.bigDecimal))
            * p._2 * p._1
        )
        .sum
    }.zip(evidenceStats)
      .par
      .map { evs =>
        (evs._1 / evs._2 - BigDecimal(
          DefaultBigDecimalMath.log(evs._2.bigDecimal)
        )).doubleValue
      }
      .toList
    catch {
      case ex: Throwable =>
        println(s"Error processing entropy stats: ${ex.getMessage} $ex")
        List(0d)
    }

  def isConverged(
      iterationCount: Int,
      numberSamplePoints: Int
  ): Boolean =
    try if (negativeEntropyStats.nonEmpty) {
      iterationCount > 1000 &&
      iterationCount >= 10 * numberSamplePoints * negativeEntropyStats.max
    } else {
      false
    } catch {
      case _: Throwable => false
    }

}

object SamplingTelemetry {

  val empty: SamplingTelemetry =
    SamplingTelemetry(List(), List())
}
