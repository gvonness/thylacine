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

import thylacine.model.core.computation.Erratum.UnexpectedErratum
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._

import cats.effect.kernel.Async
import ch.obermuhlner.math.big.DefaultBigDecimalMath

import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success, Try}

private[thylacine] case class QuadratureIntegrator[F[_]: Async](
    logPdfs: Vector[Double],
    quadratures: Vector[Vector[Double]]
) {

  private lazy val minLogPdf: BigDecimal = (logPdfs.max + logPdfs.min) / 2.0

  private lazy val integrandGraphs: Vector[Vector[(BigDecimal, BigDecimal)]] =
    quadratures.map(
      _.zip(logPdfs).map(i => (BigDecimal(i._1.toString), BigDecimal((i._2 - minLogPdf).toString)))
    )

  // Can be used in a number of places and is worth memoizing
  private[thylacine] lazy val evidenceStats: Vector[BigDecimal] =
    integrandGraphs.map { ig =>
      ig.par
        .map(p => BigDecimal(DefaultBigDecimalMath.exp(p._2.bigDecimal)) * p._1)
        .sum
    }

  // Can be used in a number of places and is worth memoizing
  private[thylacine] lazy val negativeEntropyStats: ResultOrErrF[F, Vector[BigDecimal]] =
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
        value.toResultM
      case Failure(ex) =>
        UnexpectedErratum(
          s"Error processing entropy stats: ${ex.getMessage} $ex"
        ).toResultM
    }

  private[thylacine] def getIntegrationStats(
      integrand: BigDecimal => BigDecimal
  ): ResultOrErrF[F, Vector[BigDecimal]] =
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
        value.toResultM
      case Failure(ex) =>
        UnexpectedErratum(
          s"Error processing entropy stats: ${ex.getMessage} $ex"
        ).toResultM
    }
}

private[thylacine] object QuadratureIntegrator {

  private[thylacine] def empty[F[_]: Async]: QuadratureIntegrator[F] =
    QuadratureIntegrator(Vector(), Vector())
}
