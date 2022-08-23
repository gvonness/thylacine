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

import thylacine.model.core.Erratum.ResultOrErrF.Implicits._
import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._

import cats.effect.kernel.Async

private[thylacine] abstract class IndexedCollection[F[_]: Async, T <: Container] {
  private[thylacine] def index: Map[ModelParameterIdentifier, T]

  private[thylacine] def retrieveIndex(identifier: ModelParameterIdentifier): ResultOrErrF[F, T] =
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

  private[thylacine] def getSortedValues: ResultOrErrF[F, List[T]] =
    index.toList.sortBy(_._1).map(_._2).toResultM
}
