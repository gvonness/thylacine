/*
 * Copyright 2023 Greg von Nessi
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

import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.util.MathOps

import ch.obermuhlner.math.big.DefaultBigDecimalMath

import scala.util.Random

sealed private[thylacine] trait SamplingSimulation {
  private[thylacine] def getSample: ModelParameterCollection
  private[thylacine] def isConstructed: Boolean
}

private[thylacine] object SamplingSimulation {

  private[thylacine] case object SamplingSimulationDeconstructed extends SamplingSimulation {
    final override private[thylacine] val isConstructed: Boolean = false

    final override private[thylacine] def getSample: ModelParameterCollection =
      throw new RuntimeException("Sampling simulation not constructed yet!")
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

    private val sampleStaircases: Vector[Vector[(BigDecimal, BigDecimal)]] =
      samplingWeights.map(MathOps.cdfStaircase)

    private val indexedStaircase: Map[Int, Vector[((BigDecimal, BigDecimal), Int)]] =
      (1 to numberOfAbscissas)
        .zip(
          sampleStaircases
            .map(_.zip(1 to numberOfResults))
        )
        .toMap

    private val random = Random

    final override private[thylacine] val isConstructed: Boolean = true

    override private[thylacine] def getSample: ModelParameterCollection = {
      lazy val continuousRandom = BigDecimal(Math.random().toString)

      indexedStaircase(random.nextInt(numberOfAbscissas) + 1)
        .find {
          case ((staircaseLower, staircaseUpper), _)
              if staircaseLower <= continuousRandom && staircaseUpper > continuousRandom =>
            true
          case _ =>
            false
        }
        .map { case (_, index) =>
          indexedModelParameters(index)
        }
        .get
    }
  }

  private[thylacine] val empty: SamplingSimulation = SamplingSimulationDeconstructed
}
