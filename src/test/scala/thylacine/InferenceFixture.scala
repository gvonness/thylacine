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

import bengal.stm.STM
import thylacine.config.{HookeAndJeevesConfig, MdsConfig}
import thylacine.model.components.likelihood.GaussianLinearLikelihood
import thylacine.model.components.posterior.{
  GaussianAnalyticPosterior,
  HookeAndJeevesOptimisedPosterior,
  MdsOptimisedPosterior,
  UnnormalisedPosterior
}
import thylacine.model.components.prior.{GaussianPrior, UniformPrior}

import cats.effect.IO

object InferenceFixture {

  val fooPrior: GaussianPrior[IO] =
    GaussianPrior.fromConfidenceIntervals[IO](
      label = "foo",
      values = Vector(1, 2),
      confidenceIntervals = Vector(3, 5)
    )

  val fooUniformPrior: UniformPrior[IO] =
    UniformPrior.fromBounds[IO](
      label = "fooniform",
      maxBounds = Vector(5, 5),
      minBounds = Vector(-5, -5)
    )

  def fooLikeliHoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients = Vector(Vector(1, 3), Vector(2, 4)),
    measurements = Vector(7, 10),
    uncertainties = Vector(0.01, 0.01),
    prior = fooPrior,
    evalCacheDepth = None
  )

  def fooTwoLikeliHoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients = Vector(Vector(1, 3), Vector(2, 4)),
    measurements = Vector(7, 10),
    uncertainties = Vector(0.01, 0.01),
    prior = fooUniformPrior,
    evalCacheDepth = None
  )

  val barPrior: GaussianPrior[IO] =
    GaussianPrior.fromConfidenceIntervals[IO](
      label = "bar",
      values = Vector(5),
      confidenceIntervals = Vector(.1)
    )

  val barUniformPrior: UniformPrior[IO] =
    UniformPrior.fromBounds[IO](
      label = "barniform",
      maxBounds = Vector(10),
      minBounds = Vector(-10)
    )

  def barLikeliHoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients = Vector(Vector(3), Vector(4)),
    measurements = Vector(15, 20),
    uncertainties = Vector(0.00001, 0.00001),
    prior = barPrior,
    evalCacheDepth = None
  )

  def barTwoLikeliHoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients = Vector(Vector(3), Vector(4)),
    measurements = Vector(15, 20),
    uncertainties = Vector(0.00001, 0.00001),
    prior = barUniformPrior,
    evalCacheDepth = None
  )

  def analyticPosteriorF(implicit stm: STM[IO]): IO[GaussianAnalyticPosterior[IO]] =
    for {
      fooLikeliHood <- fooLikeliHoodF
      barLikeliHood <- barLikeliHoodF
      posterior <- IO {
                     GaussianAnalyticPosterior[IO](
                       priors = Set(fooPrior, barPrior),
                       likelihoods = Set(fooLikeliHood, barLikeliHood)
                     )
                   }
      _ <- posterior.init
    } yield posterior

  def unnormalisedPosteriorF(implicit stm: STM[IO]): IO[UnnormalisedPosterior[IO]] =
    for {
      fooTwoLikeliHood <- fooTwoLikeliHoodF
      barTwoLikeliHood <- barTwoLikeliHoodF
      posterior <- IO {
                     UnnormalisedPosterior[IO](
                       priors = Set(fooUniformPrior, barUniformPrior),
                       likelihoods = Set(fooTwoLikeliHood, barTwoLikeliHood)
                     )
                   }
    } yield posterior

  val hookesAndJeevesConfig: HookeAndJeevesConfig = HookeAndJeevesConfig(
    convergenceThreshold = 1e-7,
    numberOfPriorSamplesToSetScale = Some(100)
  )

  def hookeAndJeevesOptimisedPosteriorF(implicit stm: STM[IO]): IO[HookeAndJeevesOptimisedPosterior[IO]] =
    for {
      unnormalisedPosterior <- unnormalisedPosteriorF
      posterior <- HookeAndJeevesOptimisedPosterior.of[IO](
                     hookeAndJeevesConfig = hookesAndJeevesConfig,
                     posterior = unnormalisedPosterior
                   )
    } yield posterior

  val mdsConfig: MdsConfig = MdsConfig(
    convergenceThreshold = 1e-20,
    expansionMultiplier = 2.0,
    contractionMultiplier = .5,
    evaluationParallelism = 2,
    numberOfPriorSamplesToSetStartingPoint = Some(100)
  )

  def mdsOptimisedPosteriorF(implicit stm: STM[IO]): IO[MdsOptimisedPosterior[IO]] =
    for {
      unnormalisedPosterior <- unnormalisedPosteriorF
      posterior <- MdsOptimisedPosterior.of[IO](
                     mdsConfig = mdsConfig,
                     posterior = unnormalisedPosterior
                   )
    } yield posterior
}
