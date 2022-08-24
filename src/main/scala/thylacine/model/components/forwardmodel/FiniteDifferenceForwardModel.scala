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
import thylacine.model.core.computation.ResultOrErrF
import thylacine.model.core.values.{IndexedMatrixCollection, IndexedVectorCollection}

import cats.effect.kernel.Async

private[thylacine] trait FiniteDifferenceForwardModel extends ForwardModel with FiniteDifferenceJacobian {

  // Finite difference calculation for the Jacobian is relatively intensive when compared to simple evaluation of
  // the forward model. This combined with giving the freedom to split inference parameters across any number of
  // identifiers requires us to parallelize very aggressively
  private[thylacine] final def jacobianAt[F[_]: STM: Async](
      input: IndexedVectorCollection[F]
  ): ResultOrErrF[F, IndexedMatrixCollection[F]] =
    finiteDifferencejacobianAt(input)

}
