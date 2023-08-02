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
package thylacine.model.components

import bengal.stm.STM
import thylacine.config.{ ConjugateGradientConfig, CoordinateSlideConfig, HookeAndJeevesConfig, MdsConfig }
import thylacine.model.components.forwardmodel.NonLinearForwardModel
import thylacine.model.components.likelihood.{ GaussianLikelihood, GaussianLinearLikelihood }
import thylacine.model.components.posterior._
import thylacine.model.components.prior.{ GaussianPrior, UniformPrior }

import cats.effect.IO

object ComponentFixture {

  val fooPriorLabel: String        = "foo"
  val fooUniformPriorLabel: String = "fooniform"

  val fooPrior: GaussianPrior[IO] =
    GaussianPrior.fromConfidenceIntervals[IO](
      label               = fooPriorLabel,
      values              = Vector(1, 2),
      confidenceIntervals = Vector(3, 5)
    )

  val fooUniformPrior: UniformPrior[IO] =
    UniformPrior.fromBounds[IO](
      label     = fooUniformPriorLabel,
      maxBounds = Vector(5, 5),
      minBounds = Vector(-5, -5)
    )

  def fooLikelihoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients   = Vector(Vector(1, 3), Vector(2, 4)),
    measurements   = Vector(7, 10),
    uncertainties  = Vector(0.01, 0.01),
    priorLabel     = fooPriorLabel,
    evalCacheDepth = None
  )

  private def linearMapping(input: Map[String, Vector[Double]]): Vector[Double] =
    input.values.map { vec =>
      Vector(Vector(1, 3).zip(vec).map(i => i._1 * i._2).sum, Vector(2, 4).zip(vec).map(i => i._1 * i._2).sum)
    }.head

  private def fooNonAnalyticForwardModelF(implicit stm: STM[IO]): IO[NonLinearForwardModel[IO]] =
    NonLinearForwardModel.of[IO](
      evaluation         = linearMapping,
      differential       = .0001,
      domainDimensions   = Map(fooPriorLabel -> 2),
      rangeDimension     = 2,
      evalCacheDepth     = None,
      jacobianCacheDepth = None
    )

  def fooNonAnalyticLikelihoodF(implicit stm: STM[IO]): IO[GaussianLikelihood[IO, NonLinearForwardModel[IO]]] =
    for {
      nonAnalyticForwardModel <- fooNonAnalyticForwardModelF
    } yield GaussianLikelihood.from[IO, NonLinearForwardModel[IO]](
      forwardModel  = nonAnalyticForwardModel,
      measurements  = Vector(7, 10),
      uncertainties = Vector(0.01, 0.01)
    )

  def fooTwoLikelihoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients   = Vector(Vector(1, 3), Vector(2, 4)),
    measurements   = Vector(7, 10),
    uncertainties  = Vector(0.01, 0.01),
    priorLabel     = fooUniformPriorLabel,
    evalCacheDepth = None
  )

  val barPriorLabel: String        = "bar"
  val barUniformPriorLabel: String = "barniform"

  val barPrior: GaussianPrior[IO] =
    GaussianPrior.fromConfidenceIntervals[IO](
      label               = barPriorLabel,
      values              = Vector(5),
      confidenceIntervals = Vector(.1)
    )

  val barUniformPrior: UniformPrior[IO] =
    UniformPrior.fromBounds[IO](
      label     = barUniformPriorLabel,
      maxBounds = Vector(10),
      minBounds = Vector(-10)
    )

  def barLikelihoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients   = Vector(Vector(3), Vector(4)),
    measurements   = Vector(15, 20),
    uncertainties  = Vector(0.00001, 0.00001),
    priorLabel     = barPriorLabel,
    evalCacheDepth = None
  )

  def barTwoLikelihoodF(implicit stm: STM[IO]): IO[GaussianLinearLikelihood[IO]] = GaussianLinearLikelihood.of[IO](
    coefficients   = Vector(Vector(3), Vector(4)),
    measurements   = Vector(15, 20),
    uncertainties  = Vector(0.00001, 0.00001),
    priorLabel     = barUniformPriorLabel,
    evalCacheDepth = None
  )

  def analyticPosteriorF(implicit stm: STM[IO]): IO[GaussianAnalyticPosterior[IO]] =
    for {
      fooLikelihood <- fooLikelihoodF
      barLikelihood <- barLikelihoodF
      posterior <- IO {
                     GaussianAnalyticPosterior[IO](
                       priors      = Set(fooPrior, barPrior),
                       likelihoods = Set(fooLikelihood, barLikelihood)
                     )
                   }
      _ <- posterior.init
    } yield posterior

  def unnormalisedPosteriorF(implicit stm: STM[IO]): IO[UnnormalisedPosterior[IO]] =
    for {
      fooTwoLikelihood <- fooTwoLikelihoodF
      barTwoLikelihood <- barTwoLikelihoodF
      posterior <- IO {
                     UnnormalisedPosterior[IO](
                       priors      = Set(fooUniformPrior, barUniformPrior),
                       likelihoods = Set(fooTwoLikelihood, barTwoLikelihood)
                     )
                   }
    } yield posterior

  val hookeAndJeevesConfig: HookeAndJeevesConfig = HookeAndJeevesConfig(
    convergenceThreshold           = 1e-7,
    numberOfPriorSamplesToSetScale = Some(100)
  )

  def hookeAndJeevesOptimisedPosteriorF(implicit stm: STM[IO]): IO[HookeAndJeevesOptimisedPosterior[IO]] =
    for {
      unnormalisedPosterior <- unnormalisedPosteriorF
      posterior <- HookeAndJeevesOptimisedPosterior.of[IO](
                     hookeAndJeevesConfig    = hookeAndJeevesConfig,
                     posterior               = unnormalisedPosterior,
                     iterationUpdateCallback = _ => IO.unit,
                     isConvergedCallback     = _ => IO.unit
                   )
    } yield posterior

  val coordinateSlideConfig: CoordinateSlideConfig = CoordinateSlideConfig(
    convergenceThreshold           = 1e-7,
    goldenSectionTolerance         = 1e-10,
    lineProbeExpansionFactor       = 2.0,
    numberOfPriorSamplesToSetScale = Some(100)
  )

  def coordinateSlideOptimisedPosteriorF(implicit stm: STM[IO]): IO[CoordinateSlideOptimisedPosterior[IO]] =
    for {
      unnormalisedPosterior <- unnormalisedPosteriorF
      posterior <- CoordinateSlideOptimisedPosterior.of[IO](
                     coordinateSlideConfig   = coordinateSlideConfig,
                     posterior               = unnormalisedPosterior,
                     iterationUpdateCallback = _ => IO.unit,
                     isConvergedCallback     = _ => IO.unit
                   )
    } yield posterior

  val conjugateGradientConfig: ConjugateGradientConfig = ConjugateGradientConfig(
    convergenceThreshold     = 1e-20,
    goldenSectionTolerance   = 1e-10,
    lineProbeExpansionFactor = 2.0,
    numberOfResultsToRetain  = 100
  )

  def conjugateGradientOptimisedPosteriorF(implicit stm: STM[IO]): IO[ConjugateGradientOptimisedPosterior[IO]] =
    for {
      unnormalisedPosterior <- unnormalisedPosteriorF
    } yield ConjugateGradientOptimisedPosterior.from[IO](
      conjugateGradientConfig = conjugateGradientConfig,
      posterior               = unnormalisedPosterior,
      newMaximumCallback      = _ => IO.unit,
      isConvergedCallback     = _ => IO.unit
    )

  val mdsConfig: MdsConfig = MdsConfig(
    convergenceThreshold                   = 1e-15,
    expansionMultiplier                    = 2.0,
    contractionMultiplier                  = .5,
    numberOfPriorSamplesToSetStartingPoint = Some(100)
  )

  def mdsOptimisedPosteriorF(implicit stm: STM[IO]): IO[MdsOptimisedPosterior[IO]] =
    for {
      unnormalisedPosterior <- unnormalisedPosteriorF
      posterior <- MdsOptimisedPosterior.of[IO](
                     mdsConfig               = mdsConfig,
                     posterior               = unnormalisedPosterior,
                     iterationUpdateCallback = _ => IO.unit,
                     isConvergedCallback     = _ => IO.unit
                   )
    } yield posterior
}
