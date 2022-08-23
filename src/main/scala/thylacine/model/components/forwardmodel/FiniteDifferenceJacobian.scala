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

import thylacine.model.core.Erratum.ResultOrErrF
import thylacine.model.core._

import cats.effect.kernel.Async
import cats.effect.implicits._
import cats.syntax.all._

private[thylacine] trait FiniteDifferenceJacobian {
  protected def differential: Double

  private[thylacine] def evalAt[F[_]](
      input: IndexedVectorCollection
  ): ResultOrErrF[F, VectorContainer]

  // Finite difference calculation for the Jacobian is relatively intensive when compared to simple evaluation of
  // the forward model. This combined with giving the freedom to split inference parameters across any number of
  // identifiers requires us to parallelize very aggressively
  protected final def finiteDifferencejacobianAt[F[_]: Async](
      input: IndexedVectorCollection
  ): ResultOrErrF[F, IndexedMatrixCollection] =
    for {
      currentEval <- evalAt(input)
      result <- input
                  .rawNudgeComponents(differential)
                  .toList
                  .parTraverse { kv =>
                    (1 to kv._2.size)
                      .zip(kv._2)
                      .toList
                      .parTraverse { indexAndNudge =>
                        for {
                          diffEval <- evalAt(indexAndNudge._2)
                        } yield diffEval
                          .rawSubtract(currentEval)
                          .rawScalarProductWith(1 / differential)
                          .values
                          .map(k => (k._1, indexAndNudge._1) -> k._2)
                      }
                      .map(r => kv._1 -> MatrixContainer(r.reduce(_ ++ _), currentEval.dimension, kv._2.size))
                  }
                  .map(r => IndexedMatrixCollection(r.toMap))
    } yield result

}
