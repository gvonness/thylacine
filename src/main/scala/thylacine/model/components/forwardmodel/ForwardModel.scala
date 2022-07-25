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
package thylacine.model.components.forwardmodel

import thylacine.model.core._
import thylacine.model.core.Erratum._

import cats.effect.IO

private[thylacine] trait ForwardModel
    extends GenericMapping
    with ModelParameterRawMappings
    with CanValidate[ForwardModel] {

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def evalAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[VectorContainer]

  final def evalAt(input: Map[String, Vector[Double]]): IO[Vector[Double]] =
    ResultOrErrIo.toIo {
      for {
        result <- evalAt(IndexedVectorCollection(input))
      } yield result.scalaVector
    }

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def jacobianAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[IndexedMatrixCollection]

  final def jacobianAt(input: Map[String, Vector[Double]]): IO[Map[String, Vector[Vector[Double]]]] =
    ResultOrErrIo.toIo {
      for {
        result <- jacobianAt(IndexedVectorCollection(input))
      } yield result.genericScalaRepresentation
    }
}
