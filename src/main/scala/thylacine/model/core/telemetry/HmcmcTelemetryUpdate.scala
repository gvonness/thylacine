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
        f" // Acceptance Ratio - ${jumpAcceptances.toDouble / jumpAttempts}%1.2"
      } else {
        ""
      }
    val hamiltonianDifferentialString =
      hamiltonianDifferential.map(v => f" // exp(-dH) = ${Math.exp(-v)}%1.2").getOrElse("")

    val simulationDifferentialString =
      f" // epsilon = $simulationDifferential%1.5"

    baseString + acceptanceString + hamiltonianDifferentialString + simulationDifferentialString
  }
}
