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
package thylacine.model.integration.slq

// This class enables components of the cubical cover of the domain
// to be scaled down to drive a higher acceptance rate in the integration.
// Unfortunately, this de-syncs the quadrature from the abscissa construction
// leading to overly resolved distributions. However, without scaling the
// whole algorithm's efficiency drops off significantly for higher-dimensional
// problems. There may be ways to re-introduce the scaling, but it will probably
// only be reasonable in very specific scenarios.
private[thylacine] case class QuadratureDomainTelemetry(
    currentScaleFactor: Double,
    acceptances: Int,
    rejections: Int,
    nominalAcceptance: Double,
    minValue: Double,
    acceptancesSinceLastRebuild: Int,
    rejectionStreak: Int,
    rescalingEnabled: Boolean = false // Do not enable (see comment above)
) {
  assert(
    currentScaleFactor >= Double.MinPositiveValue && currentScaleFactor <= 1.0
  )

  // TODO: Is there a way to rigorously define this?
  private[thylacine] lazy val isConverged: Boolean = rejectionStreak >= 100000

  private[thylacine] lazy val resetForRebuild: QuadratureDomainTelemetry =
    this.copy(currentScaleFactor = 1.0,
              acceptances = 0,
              rejections = 0,
              acceptancesSinceLastRebuild = 0
    )

  private[thylacine] lazy val initiateRebuild: Boolean =
    (rejectionStreak >= 1000 && acceptancesSinceLastRebuild >= 1) || currentScaleFactor == Double.MinPositiveValue

  private[thylacine] lazy val addAcceptance: QuadratureDomainTelemetry = {
    val newAcceptance             = acceptances + 1
    val newAcceptanceSinceRebuild = acceptancesSinceLastRebuild + 1
    val newAcceptanceRatio =
      newAcceptance.toDouble / (newAcceptance + rejections)
    if (newAcceptanceRatio > nominalAcceptance) {
      if (rescalingEnabled) {
        this.copy(
          currentScaleFactor = Math
            .min(currentScaleFactor + (1.0 - currentScaleFactor) / 2.0, 1.0),
          acceptances = newAcceptance,
          acceptancesSinceLastRebuild = newAcceptanceSinceRebuild,
          rejectionStreak = 0
        )
      } else {
        this.copy(
          acceptances = newAcceptance,
          acceptancesSinceLastRebuild = newAcceptanceSinceRebuild,
          rejectionStreak = 0
        )
      }
    } else if (newAcceptanceRatio < nominalAcceptance) {
      if (rescalingEnabled) {
        this.copy(
          currentScaleFactor =
            Math.max(currentScaleFactor / 2.0, Double.MinPositiveValue),
          acceptances = newAcceptance,
          acceptancesSinceLastRebuild = newAcceptanceSinceRebuild,
          rejectionStreak = 0
        )
      } else {
        this.copy(
          acceptances = newAcceptance,
          acceptancesSinceLastRebuild = newAcceptanceSinceRebuild,
          rejectionStreak = 0
        )
      }
    } else {
      this.copy(acceptances = newAcceptance,
                acceptancesSinceLastRebuild = newAcceptanceSinceRebuild,
                rejectionStreak = 0
      )
    }
  }

  private[thylacine] lazy val addRejection: QuadratureDomainTelemetry = {
    val newRejection = rejections + 1
    val newAcceptanceRatio =
      acceptances.toDouble / (acceptances + newRejection)
    if (newAcceptanceRatio > nominalAcceptance) {
      this.copy(
//        currentScaleFactor =
//          Math.min(currentScaleFactor + (1.0 - currentScaleFactor) / 2.0, 1.0),
        rejections = newRejection,
        rejectionStreak = rejectionStreak + 1
      )
    } else if (newAcceptanceRatio < nominalAcceptance) {
      this.copy(
//        currentScaleFactor =
//          Math.max(currentScaleFactor / 2.0, Double.MinPositiveValue),
        rejections = newRejection,
        rejectionStreak = rejectionStreak + 1
      )
    } else {
      this
        .copy(rejections = newRejection, rejectionStreak = rejectionStreak + 1)
    }
  }
}

private[thylacine] object QuadratureDomainTelemetry {

  private[thylacine] val init: QuadratureDomainTelemetry =
    QuadratureDomainTelemetry(
      currentScaleFactor = 1.0,
      acceptances = 0,
      rejections = 0,
      nominalAcceptance = 0,
      minValue = 0.0001,
      acceptancesSinceLastRebuild = 0,
      rejectionStreak = 0
    )
}
