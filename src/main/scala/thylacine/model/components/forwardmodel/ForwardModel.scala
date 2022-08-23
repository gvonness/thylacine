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

import bengal.stm.STM
import thylacine.model.core.Erratum.ResultOrErrF.Implicits._
import thylacine.model.core.Erratum._
import thylacine.model.core._

import cats.effect.kernel.Async

private[thylacine] trait ForwardModel
    extends GenericMapping
    with ModelParameterRawMappings
    with CanValidate[ForwardModel] {

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def evalAt[F[_]: STM: Async](
      input: IndexedVectorCollection
  ): ResultOrErrF[F, VectorContainer]

  final def evalAt[F[_]: STM: Async](input: Map[String, Vector[Double]]): F[Vector[Double]] =
    evalAt(IndexedVectorCollection(input))
      .map(_.scalaVector)
      .liftToF

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def jacobianAt[F[_]: STM: Async](
      input: IndexedVectorCollection
  ): ResultOrErrF[F, IndexedMatrixCollection]

  final def jacobianAt[F[_]: STM: Async](input: Map[String, Vector[Double]]): F[Map[String, Vector[Vector[Double]]]] =
    jacobianAt(IndexedVectorCollection(input))
      .map(_.genericScalaRepresentation)
      .liftToF
}
