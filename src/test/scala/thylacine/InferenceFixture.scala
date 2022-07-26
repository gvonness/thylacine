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
package thylacine

import thylacine.config.HookeAndJeevesConfig
import thylacine.implicits.stm
import thylacine.model.components.likelihood.GaussianLinearLikelihood
import thylacine.model.components.posterior.{GaussianAnalyticPosterior, UnNormalisedPosterior}
import thylacine.model.components.prior.{GaussianPrior, UniformPrior}
import thylacine.model.optimization.HookeAndJeevesOptimisedPosterior

import cats.effect.unsafe.implicits.global

trait InferenceFixture {

  val fooPrior: GaussianPrior =
    GaussianPrior.ofConfidenceIntervals(
      label = "foo",
      values = Vector(1, 2),
      confidenceIntervals = Vector(3, 5)
    )

  val fooUniformPrior: UniformPrior =
    UniformPrior(
      label = "fooniform",
      maxBounds = Vector(5, 5),
      minBounds = Vector(-5, -5)
    )

  val fooLikeliHood: GaussianLinearLikelihood = GaussianLinearLikelihood(
    coefficients = Vector(Vector(1, 3), Vector(2, 4)),
    measurements = Vector(7, 10),
    uncertainties = Vector(0.01, 0.01),
    prior = fooPrior,
    maxResultsToCache = 0
  )

  val fooTwoLikeliHood: GaussianLinearLikelihood = GaussianLinearLikelihood(
    coefficients = Vector(Vector(1, 3), Vector(2, 4)),
    measurements = Vector(7, 10),
    uncertainties = Vector(0.01, 0.01),
    prior = fooUniformPrior,
    maxResultsToCache = 0
  )

  val barPrior: GaussianPrior =
    GaussianPrior.ofConfidenceIntervals(
      label = "bar",
      values = Vector(5),
      confidenceIntervals = Vector(.1)
    )

  val barUniformPrior: UniformPrior =
    UniformPrior(
      label = "barniform",
      maxBounds = Vector(10),
      minBounds = Vector(-10)
    )

  val barLikeliHood: GaussianLinearLikelihood = GaussianLinearLikelihood(
    coefficients = Vector(Vector(3), Vector(4)),
    measurements = Vector(15, 20),
    uncertainties = Vector(0.00001, 0.00001),
    prior = barPrior,
    maxResultsToCache = 0
  )

  val barTwoLikeliHood: GaussianLinearLikelihood = GaussianLinearLikelihood(
    coefficients = Vector(Vector(3), Vector(4)),
    measurements = Vector(15, 20),
    uncertainties = Vector(0.00001, 0.00001),
    prior = barUniformPrior,
    maxResultsToCache = 0
  )

  val analyticPosterior: GaussianAnalyticPosterior = GaussianAnalyticPosterior(
    priors = Set(fooPrior, barPrior),
    likelihoods = Set(fooLikeliHood, barLikeliHood)
  )

  analyticPosterior.init().unsafeRunSync()

  val unNormalisedPosterior: UnNormalisedPosterior = UnNormalisedPosterior(
    priors = Set(fooUniformPrior, barUniformPrior),
    likelihoods = Set(fooTwoLikeliHood, barTwoLikeliHood)
  )

  val hookesAndJeevesConfig: HookeAndJeevesConfig = HookeAndJeevesConfig(
    convergenceThreshold = 1e-7,
    numberOfPriorSamplesToSetScale = Some(100)
  )

  val posteriorOptimiser: HookeAndJeevesOptimisedPosterior =
    HookeAndJeevesOptimisedPosterior(
      hookeAndJeevesConfig = hookesAndJeevesConfig,
      posterior = unNormalisedPosterior
    )
}
