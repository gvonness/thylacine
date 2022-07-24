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
import thylacine.util.MathOps.trapezoidalQuadrature

import cats.data.NonEmptyVector
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

case class CartesianSurface(
    xAbscissa: Vector[Double],
    yAbscissa: Vector[Double],
    progressSetCallback: Int => Unit,
    progressIncrementCallback: Unit => Unit,
    progressFinishCallback: Unit => Unit
)(implicit val stm: STM[IO]) {

  import stm._

  progressSetCallback(0)

  private val xScale = xAbscissa.max - xAbscissa.min
  private val yScale = yAbscissa.max - yAbscissa.min

  private val scalarValues =
    TxnVar.of(Map[GraphPoint, Double]()).unsafeRunSync()

  private val keys: Vector[GraphPoint] =
    xAbscissa.flatMap { x =>
      yAbscissa.map { y =>
        GraphPoint(x, y)
      }
    }

  private val normaliseColumns: Txn[Unit] = {
    for {
      values <- scalarValues.get
      groupMap = values.groupBy(_._1.x).toVector match {
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
                     }.map(_.reduce).value.unsafeRunSync()
                   case _ =>
                     Right(Seq())
                 }
      _ <- groupMap match {
             case Right(res) =>
               scalarValues.set(res.toMap)
             case Left(erratum) =>
               stm.abort(new RuntimeException(erratum.message))
           }
    } yield ()
  }

  private val zeroValuesSpec =
    keys.parTraverse { p =>
      scalarValues.modify(i => i + (p -> 0d)).commit.map(_ => ())
    }

  private def addSimplexChain(
      input: SimplexChain,
      kernelVariance: Double
  ): IO[Unit] =
    scalarValues.modify { pvs =>
      Map(
        concatenateMapping(pvs, input, kernelVariance).unsafeRunSync(): _*
      )
    }.commit

  private def addValues(
      abcissa: Vector[Double],
      values: Vector[Double],
      ds: Double,
      kernelVariance: Double
  ): IO[Unit] =
    for {
      _ <- IO(progressIncrementCallback(()))
      chain <- IO(
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
  ): IO[Vector[(GraphPoint, Double)]] =
    startMap.toVector match {
      case h +: t =>
        NonEmptyVector(h, t).parTraverse { pv =>
          IO {
            pv._1 -> chain.getPoints.foldLeft(pv._2) { (i, j) =>
              i + Math.exp(
                -0.5 * j.scaledDistSquaredTo(pv._1, xScale, yScale) / kernelVariance
              )
            }
          }
        }.map(_.toVector)
      case _ => IO.pure(Vector[(GraphPoint, Double)]())
    }

  def addSamples(
      abcissa: Vector[Double],
      samples: Vector[Vector[Double]],
      ds: Double,
      kernelVariance: Double
  ): IO[Unit] =
    samples match {
      case h +: t =>
        for {
          _ <- IO(progressSetCallback(samples.size))
          _ <-
            NonEmptyVector(h, t)
              .traverse(i => addValues(abcissa, i, ds, kernelVariance))
          _ <- IO(progressIncrementCallback)
          _ <- IO(progressFinishCallback(()))
        } yield ()
      case _ =>
        IO.unit
    }

  def getResults: IO[Map[(Double, Double), Double]] =
    (for {
      _        <- normaliseColumns
      plotData <- scalarValues.get
    } yield plotData.map(i => i._1.primitiveValue -> i._2)).commit

  zeroValuesSpec.unsafeRunSync()
}
