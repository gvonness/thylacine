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

case class SamplingAbscissaCollection(
    abscissas: List[SamplingAbscissa]
) {
  assert(abscissas.map(_.abscissa.size).toSet.size <= 1)

  val size: Int =
    if (abscissas.nonEmpty) abscissas.head.abscissa.size else 0

  lazy val extendAllAbscissaByOne: SamplingAbscissaCollection =
    SamplingAbscissaCollection(abscissas.map(_.extendAbscissaByOne))

  lazy val getQuadratures: List[List[Double]] =
    abscissas.map(_.getTrapezoidalQuadrature)

  lazy val getAbscissas: List[List[Double]] =
    abscissas.map(_.abscissa)
}

object SamplingAbscissaCollection {

  def apply(
      numberOfAbscissas: Int,
      numberOfSamplesPerAbscissa: Int
  ): SamplingAbscissaCollection = {
    val abscissas: List[SamplingAbscissa] =
      (1 to numberOfAbscissas)
        .map(_ => SamplingAbscissa(numberOfSamplesPerAbscissa))
        .toList

    SamplingAbscissaCollection(abscissas = abscissas)
  }

  val init: SamplingAbscissaCollection = apply(0, 0)
}
