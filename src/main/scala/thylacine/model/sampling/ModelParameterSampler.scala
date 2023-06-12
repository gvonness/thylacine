/*
 * Copyright 2020-2023 Greg von Nessi
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
package thylacine.model.sampling

import thylacine.model.core.AsyncImplicits
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.core.values.VectorContainer

import cats.syntax.all._

private[thylacine] trait ModelParameterSampler[F[_]] {
  this: AsyncImplicits[F] =>
  protected def sampleModelParameters: F[ModelParameterCollection]

  // Low-level API - For sampling priors
  protected def rawSampleModelParameters: F[VectorContainer]

  final def sample: F[Map[String, Vector[Double]]] =
    sampleModelParameters.map(_.genericScalaRepresentation)
}
