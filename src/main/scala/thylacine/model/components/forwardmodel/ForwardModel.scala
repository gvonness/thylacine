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
package thylacine.model.components.forwardmodel

import thylacine.model.core.*
import thylacine.model.core.values.*

import cats.syntax.all.*

trait ForwardModel[F[_]] extends GenericMapping with CanValidate[ForwardModel[F]] {
  this: AsyncImplicits[F] =>

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def evalAt(
    input: IndexedVectorCollection
  ): F[VectorContainer]

  final private[thylacine] def evalAt(input: Map[String, Vector[Double]]): F[Vector[Double]] =
    evalAt(IndexedVectorCollection(input)).map(_.scalaVector)

  // Note that input validation should be done within
  // the surrounding likelihood
  private[thylacine] def jacobianAt(
    input: IndexedVectorCollection
  ): F[IndexedMatrixCollection]

  final private[thylacine] def jacobianAt(input: Map[String, Vector[Double]]): F[Map[String, Vector[Vector[Double]]]] =
    jacobianAt(IndexedVectorCollection(input))
      .map(_.genericScalaRepresentation)
}
