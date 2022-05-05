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
package thylacine.model.integration.slq

import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.util.MathOps

import cats.implicits.toTraverseOps
import ch.obermuhlner.math.big.DefaultBigDecimalMath

import scala.util.Random

sealed trait SamplingSimulation {
  def getSample: ResultOrErrIo[ModelParameterCollection]
  def isConstructed: Boolean
}

object SamplingSimulation {

  case object SamplingSimulationUnconstructed extends SamplingSimulation {
    override final val isConstructed: Boolean = false

    override final def getSample: ResultOrErrIo[ModelParameterCollection] =
      ResultOrErrIo.fromErratum(
        UnexpectedErratum("Sampling simulation not constructed yet!")
      )
  }

  case class SamplingSimulationConstructed(
      logPdfResults: List[(Double, ModelParameterCollection)],
      abscissas: List[List[Double]]
  ) extends SamplingSimulation {
    private val numberOfResults   = logPdfResults.size
    private val numberOfAbscissas = abscissas.size

    private val pdfResults: Seq[BigDecimal] = logPdfResults
      .map(l =>
        BigDecimal(
          DefaultBigDecimalMath.exp(BigDecimal(l._1.toString).bigDecimal)
        )
      )

    private val samplingWeights: List[List[BigDecimal]] =
      abscissas
        .map(_.zip(pdfResults))
        .map { ig =>
          ((0d, ig.head._2) :: ig.dropRight(1)).zip(ig).map { avs =>
            val ((x1, f1), (x2, f2)): (
                (Double, BigDecimal),
                (Double, BigDecimal)
            ) = avs
            BigDecimal("0.5") * (f2 + f1) * BigDecimal((x2 - x1).toString)
          }
        }

    private val indexedModelParameters: Map[Int, ModelParameterCollection] =
      (1 to numberOfResults).zip(logPdfResults.map(_._2)).toMap

    private val sampleStaircases
        : ResultOrErrIo[List[List[(BigDecimal, BigDecimal)]]] =
      samplingWeights.traverse(MathOps.cdfStaircase)

    private val indexedStaircase
        : ResultOrErrIo[Map[Int, List[((BigDecimal, BigDecimal), Int)]]] =
      for {
        cdfStaircases <- sampleStaircases
      } yield (1 to numberOfAbscissas)
        .zip(cdfStaircases.map(_.zip(1 to numberOfResults)))
        .toMap

    private val random = Random

    override final val isConstructed: Boolean = true

    def getSample: ResultOrErrIo[ModelParameterCollection] =
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

  val empty: SamplingSimulation = SamplingSimulationUnconstructed
}
