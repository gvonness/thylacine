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
package thylacine.model.distributions

import thylacine.model.core.GenericScalarValuedMapping
import thylacine.model.core.values.VectorContainer

private[thylacine] trait Distribution extends GenericScalarValuedMapping {
  private[thylacine] def logPdfAt(input: VectorContainer): Double

  private[thylacine] def logPdfGradientAt(
      input: VectorContainer
  ): VectorContainer
}