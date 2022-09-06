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

import thylacine.model.core.StmImplicits
import thylacine.model.core.computation.CachedComputation
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.{IndexedMatrixCollection, VectorContainer}

private[thylacine] trait InMemoryMemoizedForwardModel[F[_]] extends ForwardModel[F] {
  this: StmImplicits[F] =>

  protected val evalCache: CachedComputation[F, VectorContainer]
  protected val jacobianCache: CachedComputation[F, IndexedMatrixCollection]

  private[thylacine] override final def evalAt(
      input: ModelParameterCollection
  ): F[VectorContainer] =
    evalCache.performComputation(input)

  private[thylacine] override def jacobianAt(
      input: ModelParameterCollection
  ): F[IndexedMatrixCollection] =
    jacobianCache.performComputation(input)
}
