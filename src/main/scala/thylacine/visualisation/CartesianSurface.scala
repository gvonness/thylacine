/*
 * Copyright 2020-2021 Entrolution
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

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

case class CartesianSurface(
    xAbscissa: List[Double],
    yAbscissa: List[Double]
) {

  val stm: STM[IO] = STM.runtime[IO].unsafeRunSync()
  import stm._

  private val xScale = xAbscissa.max - xAbscissa.min
  private val yScale = yAbscissa.max - yAbscissa.min

  private val scalarValues =
    TxnVar.of(Map[GraphPoint, Double]()).unsafeRunSync()

  private val keys: List[GraphPoint] =
    xAbscissa.flatMap { x =>
      yAbscissa.map { y =>
        GraphPoint(x, y)
      }
    }

  val normaliseColumns: Txn[Unit] = {
    for {
      values <- scalarValues.get
      groupMap = values.groupBy(_._1.x).toList match {
                   case h :: t =>
                     NonEmptyList(h, t).parTraverse { col =>
                       for {
                         integration <- IO(
                                          trapezoidalRule(col._2.values.toList,
                                                          yAbscissa.differential
                                          )
                                        )
                       } yield
                         if (integration > 0) {
                           col._2.view.mapValues(v => v / integration).toSeq
                         } else {
                           col._2.toSeq
                         }
                     }.map(_.reduce).unsafeRunSync()
                   case _ =>
                     Seq()
                 }
      _ <- scalarValues.set(groupMap.toMap)
    } yield ()
  }

  private val zeroValuesSpec =
    keys.parTraverse { p =>
      scalarValues.modify(i => i + (p -> 0d)).commit.map(_ => ())
    }

  def addSamples(
      abcissa: List[Double],
      samples: List[List[Double]],
      ds: Double,
      kernelVariance: Double
  ): IO[Unit] =
    samples match {
      case h :: t =>
        for {
          _ <-
            NonEmptyList(h, t)
              .traverse(i => addValues(abcissa, i, ds, kernelVariance))
        } yield ()
      case _ =>
        IO.unit
    }

  private def addValues(
      abcissa: List[Double],
      values: List[Double],
      ds: Double,
      kernelVariance: Double
  ): IO[Unit] =
    for {
      chain <- IO(
                 SimplexChain(
                   abcissa.zip(values).map(GraphPoint(_))
                 ).reinterp(ds)
               )
      _ <- addSimplexChain(chain, kernelVariance)
    } yield ()

  private def addSimplexChain(
      input: SimplexChain,
      kernelVariance: Double
  ): IO[Unit] =
    scalarValues.modify { pvs =>
      Map(
        concatenateMapping(pvs, input, kernelVariance).unsafeRunSync(): _*
      )
    }.commit

  private def concatenateMapping(
      startMap: Map[GraphPoint, Double],
      chain: SimplexChain,
      kernelVariance: Double
  ): IO[List[(GraphPoint, Double)]] =
    startMap.toList match {
      case h :: t =>
        NonEmptyList(h, t).parTraverse { pv =>
          IO {
            pv._1 -> chain.getPoints.foldLeft(pv._2) { (i, j) =>
              i + Math.exp(
                -0.5 * j.scaledDistSquaredTo(pv._1,
                                             xScale,
                                             yScale
                ) / kernelVariance
              )
            }
          }
        }.map(_.toList)
      case _ => IO.pure(List[(GraphPoint, Double)]())
    }

  def getResults: IO[Map[GraphPoint, Double]] =
    (for {
      _        <- normaliseColumns
      plotData <- scalarValues.get
    } yield plotData).commit

  zeroValuesSpec.unsafeRunSync()
}
