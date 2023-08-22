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
package thylacine.model.core.telemetry

case class HmcmcTelemetryUpdate(
  samplesRemaining: Int,
  jumpAttempts: Int,
  jumpAcceptances: Int,
  simulationDifferential: Double,
  hamiltonianDifferential: Option[Double]
) extends TelemetryReport {

  override lazy val logMessage: String = {
    val baseString =
      s"HMCMC Sampling :: Samples remaining - $samplesRemaining"
    val acceptanceString =
      if (jumpAttempts != 0) {
        s" // Acceptance Ratio - ${jumpAcceptances.toDouble / jumpAttempts}"
      } else {
        ""
      }
    val hamiltonianDifferentialString =
      hamiltonianDifferential.map(v => s" // exp(-dH) = ${Math.exp(-v)}").getOrElse("")

    val simulationDifferentialString =
      s" // epsilon = $simulationDifferential"

    baseString + acceptanceString + hamiltonianDifferentialString + simulationDifferentialString
  }
}
