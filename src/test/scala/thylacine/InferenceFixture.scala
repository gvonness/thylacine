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
    prior = fooPrior
  )

  val fooTwoLikeliHood: GaussianLinearLikelihood = GaussianLinearLikelihood(
    coefficients = Vector(Vector(1, 3), Vector(2, 4)),
    measurements = Vector(7, 10),
    uncertainties = Vector(0.01, 0.01),
    prior = fooUniformPrior
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
    prior = barPrior
  )

  val barTwoLikeliHood: GaussianLinearLikelihood = GaussianLinearLikelihood(
    coefficients = Vector(Vector(3), Vector(4)),
    measurements = Vector(15, 20),
    uncertainties = Vector(0.00001, 0.00001),
    prior = barUniformPrior
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
