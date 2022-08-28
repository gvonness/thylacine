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
package thylacine.model.core.values

import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.computation.Erratum.UnexpectedErratum
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.computation.ResultOrErrF.Implicits._

import cats.effect.kernel.Async

private[thylacine] trait IndexedCollection[T <: Container] {
  private[thylacine] def index: Map[ModelParameterIdentifier, T]

  private[thylacine] def retrieveIndex[F[_]: Async](identifier: ModelParameterIdentifier): ResultOrErrF[F, T] =
    index
      .get(identifier)
      .map(Right(_))
      .getOrElse(
        Left(
          UnexpectedErratum(
            s"Identifier $identifier not found in indexed collection: $index"
          )
        )
      )
      .toResultM

  private[thylacine] def getSortedValues[F[_]: Async]: ResultOrErrF[F, List[T]] =
    index.toList
      .sortBy(_._1)
      .map(_._2)
      .toResultM
}
