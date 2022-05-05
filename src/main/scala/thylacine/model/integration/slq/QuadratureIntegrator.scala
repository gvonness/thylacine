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

import thylacine.model.core.Erratum._
import ch.obermuhlner.math.big.DefaultBigDecimalMath

import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success, Try}

private[thylacine] case class QuadratureIntegrator(
    logPdfs: Vector[Double],
    quadratures: Vector[Vector[Double]]
) {

  private lazy val minLogPdf: BigDecimal = (logPdfs.max + logPdfs.min) / 2.0

  private lazy val integrandGraphs: Vector[Vector[(BigDecimal, BigDecimal)]] =
    quadratures.map(
      _.zip(logPdfs).map(i =>
        (BigDecimal(i._1.toString), BigDecimal((i._2 - minLogPdf).toString))
      )
    )

  // Can be used in a number of places and is worth memoizing
  private[thylacine] lazy val evidenceStats: Vector[BigDecimal] =
    integrandGraphs.map { ig =>
      ig.par
        .map(p => BigDecimal(DefaultBigDecimalMath.exp(p._2.bigDecimal)) * p._1)
        .sum
    }

  // Can be used in a number of places and is worth memoizing
  private[thylacine] lazy val negativeEntropyStats
      : ResultOrErrIo[Vector[BigDecimal]] =
    Try {
      integrandGraphs.map { ig =>
        ig.par
          .map(p =>
            BigDecimal(DefaultBigDecimalMath.exp(p._2.bigDecimal))
              * p._2 * p._1
          )
          .sum
      }.zip(evidenceStats)
        .par
        .map { evs =>
          evs._1 / evs._2 - BigDecimal(
            DefaultBigDecimalMath.log(evs._2.bigDecimal)
          )
        }
        .toVector
    } match {
      case Success(value) =>
        ResultOrErrIo.fromValue(value)
      case Failure(ex) =>
        ResultOrErrIo.fromErratum(
          UnexpectedErratum(
            s"Error processing entropy stats: ${ex.getMessage} $ex"
          )
        )
    }

  private[thylacine] def getIntegrationStats(
      integrand: BigDecimal => BigDecimal
  ): ResultOrErrIo[Vector[BigDecimal]] =
    Try {
      integrandGraphs.map { ig =>
        ig.par
          .map(p =>
            integrand(BigDecimal(DefaultBigDecimalMath.exp(p._2.bigDecimal)))
              * p._1
          )
          .sum
      }.zip(evidenceStats)
        .par
        .map { evs =>
          evs._1 / evs._2 - BigDecimal(
            DefaultBigDecimalMath.log(evs._2.bigDecimal)
          )
        }
        .toVector
    } match {
      case Success(value) =>
        ResultOrErrIo.fromValue(value)
      case Failure(ex) =>
        ResultOrErrIo.fromErratum(
          UnexpectedErratum(
            s"Error processing entropy stats: ${ex.getMessage} $ex"
          )
        )
    }
}

private[thylacine] object QuadratureIntegrator {

  private[thylacine] val empty: QuadratureIntegrator =
    QuadratureIntegrator(Vector(), Vector())
}
