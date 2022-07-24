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
package thylacine.model.core

import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._

import cats.effect.IO

private[thylacine] trait ModelParameterSampler {

  protected def sampleModelParameters: ResultOrErrIo[ModelParameterCollection]

  // Low-level API - For sampling priors
  protected def rawSampleModelParameters: ResultOrErrIo[VectorContainer]

  final def sample: IO[Map[String, Vector[Double]]] =
    for {
      sampleRes <- sampleModelParameters.value
      result <- sampleRes match {
                  case Right(res) =>
                    IO.pure(res)
                  case Left(erratum) =>
                    IO.raiseError(new RuntimeException(erratum.toString))
                }
    } yield result.genericScalaRepresentation
}
