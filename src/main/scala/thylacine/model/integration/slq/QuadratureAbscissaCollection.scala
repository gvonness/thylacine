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

private[thylacine] case class QuadratureAbscissaCollection(
    abscissas: Vector[QuadratureAbscissa]
) {
  assert(abscissas.map(_.abscissa.size).toSet.size <= 1)

  private[thylacine] val size: Int =
    if (abscissas.nonEmpty) abscissas.head.abscissa.size else 0

  private[thylacine] lazy val extendAllAbscissaByOne: QuadratureAbscissaCollection =
    QuadratureAbscissaCollection(abscissas.map(_.extendAbscissaByOne))

  private[thylacine] lazy val getQuadratures: Vector[Vector[Double]] =
    abscissas.map(_.getTrapezoidalQuadrature)

  private[thylacine] lazy val getAbscissas: Vector[Vector[Double]] =
    abscissas.map(_.abscissa)
}

private[thylacine] object QuadratureAbscissaCollection {

  private[thylacine] def apply(
      numberOfAbscissas: Int,
      numberOfSamplesPerAbscissa: Int
  ): QuadratureAbscissaCollection = {
    val abscissas: Vector[QuadratureAbscissa] =
      (1 to numberOfAbscissas)
        .map(_ => QuadratureAbscissa(numberOfSamplesPerAbscissa))
        .toVector

    QuadratureAbscissaCollection(abscissas = abscissas)
  }

  private[thylacine] val init: QuadratureAbscissaCollection = apply(0, 0)
}
