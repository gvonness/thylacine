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

import thylacine.model.core.Erratum.{ResultOrErrIo, _}
import thylacine.model.core.IndexedVectorCollection._
import thylacine.util.MathOps

import cats.implicits.toTraverseOps
import ch.obermuhlner.math.big.DefaultBigDecimalMath

import scala.util.Random

private[thylacine] sealed trait SamplingSimulation {
  private[thylacine] def getSample: ResultOrErrIo[ModelParameterCollection]
  private[thylacine] def isConstructed: Boolean
}

private[thylacine] object SamplingSimulation {

  private[thylacine] case object SamplingSimulationUnconstructed extends SamplingSimulation {
    private[thylacine] override final val isConstructed: Boolean = false

    private[thylacine] override final def getSample: ResultOrErrIo[ModelParameterCollection] =
      ResultOrErrIo.fromErratum(
        UnexpectedErratum("Sampling simulation not constructed yet!")
      )
  }

  private[thylacine] case class SamplingSimulationConstructed(
      logPdfResults: Vector[(Double, ModelParameterCollection)],
      abscissas: Vector[Vector[Double]]
  ) extends SamplingSimulation {
    private val numberOfResults   = logPdfResults.size
    private val numberOfAbscissas = abscissas.size

    private val pdfResults: Seq[BigDecimal] = logPdfResults
      .map(l =>
        BigDecimal(
          DefaultBigDecimalMath.exp(BigDecimal(l._1.toString).bigDecimal)
        )
      )

    private val samplingWeights: Vector[Vector[BigDecimal]] =
      abscissas
        .map(_.zip(pdfResults))
        .map { ig =>
          ((0d, ig.head._2) +: ig.dropRight(1)).zip(ig).map { avs =>
            val ((x1, f1), (x2, f2)): (
                (Double, BigDecimal),
                (Double, BigDecimal)
            ) = avs
            BigDecimal("0.5") * (f2 + f1) * BigDecimal((x2 - x1).toString)
          }
        }

    private val indexedModelParameters: Map[Int, ModelParameterCollection] =
      (1 to numberOfResults).zip(logPdfResults.map(_._2)).toMap

    private val sampleStaircases: ResultOrErrIo[Vector[Vector[(BigDecimal, BigDecimal)]]] =
      samplingWeights.traverse(MathOps.cdfStaircase)

    private val indexedStaircase: ResultOrErrIo[Map[Int, Vector[((BigDecimal, BigDecimal), Int)]]] =
      for {
        cdfStaircases <- sampleStaircases
      } yield (1 to numberOfAbscissas)
        .zip(cdfStaircases.map(_.zip(1 to numberOfResults)))
        .toMap

    private val random = Random

    private[thylacine] override final val isConstructed: Boolean = true

    private[thylacine] override def getSample: ResultOrErrIo[ModelParameterCollection] =
      indexedStaircase.flatMap { cdfStaircase =>
        ResultOrErrIo.fromResultOrErr {
          cdfStaircase
            .get(random.nextInt(numberOfAbscissas) + 1)
            .flatMap { scs =>
              val continuousRandom = BigDecimal(Math.random().toString)
              scs.find { sc =>
                sc._1._1 <= continuousRandom && sc._1._2 > continuousRandom
              }.map(_._2)
            }
            .flatMap(indexedModelParameters.get)
            .map(Right(_))
            .getOrElse(
              Left(UnexpectedErratum("Failed to generated simulated sample!"))
            )
        }
      }
  }

  private[thylacine] val empty: SamplingSimulation = SamplingSimulationUnconstructed
}
