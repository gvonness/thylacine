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
package thylacine.visualisation

import bengal.stm._
import bengal.stm.model._
import bengal.stm.syntax.all._
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.util.MathOps.trapezoidalQuadrature

import cats.data.NonEmptyVector
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._

case class CartesianSurface[F[_]: STM: Async](
    xAbscissa: Vector[Double],
    yAbscissa: Vector[Double],
    progressSetCallback: Int => Unit,
    progressIncrementCallback: Unit => Unit,
    progressFinishCallback: Unit => Unit
)(private val scalarValues: TxnVar[F, Map[GraphPoint, Double]]) {

  progressSetCallback(0)

  private val xScale = xAbscissa.max - xAbscissa.min
  private val yScale = yAbscissa.max - yAbscissa.min

  private val keys: Vector[GraphPoint] =
    xAbscissa.flatMap { x =>
      yAbscissa.map { y =>
        GraphPoint(x, y)
      }
    }

  private val normaliseColumns: Txn[Unit] = {
    for {
      values <- scalarValues.get
      groupMap = (values.groupBy(_._1.x).toVector match {
                   case h +: t =>
                     NonEmptyVector(h, t).parTraverse { col =>
                       for {
                         integration <- trapezoidalQuadrature(
                                          yAbscissa,
                                          col._2.values.toVector
                                        )
                         //integration <- ResultOrErrIo.fromCalculation(col._2.values.toVector.max)
                       } yield
                         if (integration > 0) {
                           col._2.view.mapValues(v => v / integration).toSeq
                         } else {
                           col._2.toSeq
                         }
                     }.map(_.reduce).value
                   case _ =>
                     Seq[(GraphPoint, Double)]().toResultM.value
                 }).flatMap {
                   case Right(res)    => Async[F].delay(res.toMap)
                   case Left(erratum) => Async[F].raiseError(new RuntimeException(erratum.message))
                 }
      _ <- scalarValues.setF(groupMap).handleErrorWith(STM[F].abort)
    } yield ()
  }

  private val zeroValuesSpec =
    keys.traverse { p =>
      scalarValues.modify(i => i + (p -> 0d)).commit.map(_ => ())
    }

  private def addSimplexChain(
      input: SimplexChain,
      kernelVariance: Double
  ): F[Unit] =
    scalarValues.modifyF { pvs =>
      concatenateMapping(pvs, input, kernelVariance).map(_.toMap)
    }.commit

  private def addValues(
      abcissa: Vector[Double],
      values: Vector[Double],
      ds: Double,
      kernelVariance: Double
  ): F[Unit] =
    for {
      _ <- Async[F].delay(progressIncrementCallback(()))
      chain <- Async[F].delay(
                 SimplexChain(
                   abcissa.zip(values).map(GraphPoint(_))
                 ).reinterp(ds)
               )
      _ <- addSimplexChain(chain, kernelVariance)
    } yield ()

  private def concatenateMapping(
      startMap: Map[GraphPoint, Double],
      chain: SimplexChain,
      kernelVariance: Double
  ): F[Vector[(GraphPoint, Double)]] =
    startMap.toVector match {
      case h +: t =>
        NonEmptyVector(h, t).parTraverse { pv =>
          Async[F].delay {
            pv._1 -> chain.getPoints.foldLeft(pv._2) { (i, j) =>
              i + Math.exp(
                -0.5 * j.scaledDistSquaredTo(pv._1, xScale, yScale) / kernelVariance
              )
            }
          }
        }.map(_.toVector)
      case _ => Async[F].pure(Vector[(GraphPoint, Double)]())
    }

  def addSamples(
      abcissa: Vector[Double],
      samples: Vector[Vector[Double]],
      ds: Double,
      kernelVariance: Double
  ): F[Unit] =
    samples match {
      case h +: t =>
        for {
          _ <- Async[F].delay(progressSetCallback(samples.size))
          _ <-
            NonEmptyVector(h, t)
              .traverse(i => addValues(abcissa, i, ds, kernelVariance))
          _ <- Async[F].delay(progressIncrementCallback)
          _ <- Async[F].delay(progressFinishCallback(()))
        } yield ()
      case _ =>
        Async[F].unit
    }

  def getResults: F[Map[(Double, Double), Double]] =
    (for {
      _        <- normaliseColumns
      plotData <- scalarValues.get
    } yield plotData.map(i => i._1.primitiveValue -> i._2)).commit
}

object CartesianSurface {

  def of[F[_]: STM: Async](
      xAbscissa: Vector[Double],
      yAbscissa: Vector[Double],
      progressSetCallback: Int => Unit,
      progressIncrementCallback: Unit => Unit,
      progressFinishCallback: Unit => Unit
  ): F[CartesianSurface[F]] =
    for {
      scalarValues <- TxnVar.of(Map[GraphPoint, Double]())
      result <-
        Async[F].delay(
          CartesianSurface(xAbscissa,
                           yAbscissa,
                           progressSetCallback,
                           progressIncrementCallback,
                           progressFinishCallback
          )(
            scalarValues
          )
        )
      _ <- result.zeroValuesSpec
    } yield result

}
