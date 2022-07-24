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
package bayken

import bayken.config._
import bayken.config.measurements._
import bayken.model.Abscissa.UniformAbscissa
import bayken.model.ken.ShinkenGeometry
import bayken.model.measurement.{BalanceBeamExperimentProcessor, MeasurementRow}
import bayken.numerical.{Point2D, RealValuedFunction}
import bayken.util.{DataWriter, ProgressBarOps}
import bengal.stm.STM
import thylacine.implicits.stm
import thylacine.model.components.forwardmodel.{
  LinearForwardModel,
  NonLinearFiniteDifferenceInMemoryMemoizedForwardModel
}
import thylacine.model.components.likelihood.{GaussianLikelihood, GaussianLinearLikelihood}
import thylacine.model.components.posterior.{GaussianAnalyticPosterior, UnNormalisedPosterior}
import thylacine.model.components.prior.GaussianPrior
import thylacine.model.optimization.HookeAndJeevesOptimisedPosterior
import thylacine.model.sampling.hmcmc.HmcmcPosteriorSampler
import thylacine.visualisation.CartesianSurface

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.implicits._
import os.Path

object NonAnalyticInference {

  def runAndVisualize(
      kenConfig: ShinkenConfig,
      resultPath: Path
  )(implicit ior: IORuntime): Unit = {
    val progressBar: ProgressBarOps = ProgressBarOps(implicitly[STM[IO]])
    val dataWriter: DataWriter.type = DataWriter

    println(
      s"Performing non-analytic inference of mass density profile - ${kenConfig.label}"
    )

    println(
      s"Setting up inference..."
    )

    val data: ShinkenMeasurements               = kenConfig.measurements
    val rawMunePoints: Seq[BackEdgePointConfig] = data.pointMeasurements

    val bladeGeometry: ShinkenGeometry = ShinkenGeometry(
      rawMunePoints = rawMunePoints.toList,
      inferenceConfig = kenConfig.inferenceParameters
    )

    // ------------- -------- ----- --- -- - -
    // Prior Construction
    // - - -- --- ----- -------- -------------

    // We parameterise mass-per-length as piecewise polynomial coefficients
    // corresponding to the measured geometry of the blade. This model enables
    // us to account for expected jumps in mass density (due to the sword be
    // constructed of heteroenous parts) while suppressing high-frequency, deconvolution
    // noise. We use an 'effective' max-entropy (uniform) prior with bounds
    // set to easily encompass any physically-realistic parametrisation
    val massPerLengthPrior: GaussianPrior =
      GaussianPrior.ofCovariance(
        label = "mass-per-length-parametrization",
        values = bladeGeometry.gaussianProcessMean,
        covarianceMatrix = bladeGeometry.gaussianProcessCovariance
      )

    // ------------- -------- ----- --- -- - -
    // Likelihood Construction
    // - - -- --- ----- -------- -------------

    val experimentProcessor: BalanceBeamExperimentProcessor =
      BalanceBeamExperimentProcessor(bladeGeometry)

    def getAnalyticLikelihood(
        measurements: Vector[MeasurementRow]
    ): GaussianLinearLikelihood =
      GaussianLinearLikelihood(
        coefficients = measurements.map(_.rowCoefficients),
        measurements = measurements.map(_.solveConstant),
        uncertainties = measurements.map(_.uncertainty),
        prior = massPerLengthPrior
      )

    def getNonAnalyticLikelihood(measurements: Vector[MeasurementRow]) = {
      val linearMapping = LinearForwardModel(massPerLengthPrior.label, measurements.map(_.rowCoefficients))
      val nonAnalyticForwardModel = NonLinearFiniteDifferenceInMemoryMemoizedForwardModel(
        evaluation = linearMapping.evalAt(_).unsafeRunSync(),
        domainDimensions = Map(massPerLengthPrior.label -> linearMapping.domainDimension),
        rangeDimension = linearMapping.rangeDimension,
        differential = 0.0000001,
        maxResultsToCache = 1000
      )

      GaussianLikelihood(
        forwardModel = nonAnalyticForwardModel,
        measurements = measurements.map(_.solveConstant),
        uncertainties = measurements.map(_.uncertainty)
      )
    }

    val totalMassAnalyticLikelihood: GaussianLinearLikelihood =
      getAnalyticLikelihood {
        Vector(
          experimentProcessor.totalMassMeasurement(kenConfig.measurements.shinkenMass,
                                                   kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val zeroMassAtTipAnalyticLikelihood: GaussianLinearLikelihood =
      getAnalyticLikelihood {
        Vector(
          experimentProcessor.zeroMassAtTipMeasurement(
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val massContinuousAtKissakeAnalyticLikelihood: GaussianLinearLikelihood =
      getAnalyticLikelihood {
        Vector(
          experimentProcessor.massContinuousAroundKissake(
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val balanceBeamMeasurementsAnalyticLikelihood: GaussianLinearLikelihood =
      getAnalyticLikelihood {
        kenConfig.measurements.measurementModels.flatMap { mm =>
          experimentProcessor.processExperiment(mm)
        }.toVector
      }

    val totalMassNonAnalyticLikelihood: GaussianLikelihood[NonLinearFiniteDifferenceInMemoryMemoizedForwardModel] =
      getNonAnalyticLikelihood {
        Vector(
          experimentProcessor.totalMassMeasurement(kenConfig.measurements.shinkenMass,
                                                   kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val zeroMassAtTipNonAnalyticLikelihood: GaussianLikelihood[NonLinearFiniteDifferenceInMemoryMemoizedForwardModel] =
      getNonAnalyticLikelihood {
        Vector(
          experimentProcessor.zeroMassAtTipMeasurement(
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val massContinuousAtKissakeNonAnalyticLikelihood
        : GaussianLikelihood[NonLinearFiniteDifferenceInMemoryMemoizedForwardModel] =
      getNonAnalyticLikelihood {
        Vector(
          experimentProcessor.massContinuousAroundKissake(
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val balanceBeamMeasurementsNonAnalyticLikelihood
        : GaussianLikelihood[NonLinearFiniteDifferenceInMemoryMemoizedForwardModel] =
      getNonAnalyticLikelihood {
        kenConfig.measurements.measurementModels.flatMap { mm =>
          experimentProcessor.processExperiment(mm)
        }.toVector
      }

    // ------------- -------- ----- --- -- - -
    // Posterior Construction
    // - - -- --- ----- -------- -------------

    println(
      s"\nAnalytic Posterior..."
    )

    val analyticPosterior = GaussianAnalyticPosterior(
      priors = Set(massPerLengthPrior),
      likelihoods = Set(
        //        totalMassLikelihood,
        zeroMassAtTipAnalyticLikelihood,
        //        massContinuousAtKissakeLikelihood,
        balanceBeamMeasurementsAnalyticLikelihood
      )
    )

    analyticPosterior.init().unsafeRunSync()

    println(
      s"\nNon-Analytic Posterior..."
    )

    val posterior = UnNormalisedPosterior(
      priors = Set(massPerLengthPrior),
      likelihoods = Set(
        //        totalMassLikelihood,
        zeroMassAtTipAnalyticLikelihood,
        //        massContinuousAtKissakeLikelihood,
        balanceBeamMeasurementsNonAnalyticLikelihood
      )
    )

    val posteriorOptimiser: HookeAndJeevesOptimisedPosterior =
      HookeAndJeevesOptimisedPosterior(
        hookeAndJeevesConfig = kenConfig.inferenceParameters.hookesAndJeevesParameters,
        posterior = posterior,
        newMaximumCallback = nm =>
          print(
            s"\rHooke & Jeeves Optimisation: New posterior maximum found of $nm"
          ),
        newScaleCallback = ns => println(s"\nHooke & Jeeves Optimisation: Rescaling to $ns"),
        isConvergedCallback = _ => println(s"\nHooke & Jeeves Optimisation: Has converged!")
      )

    println(
      s"\nAcquiring optimisation result..."
    )

    val optimisationResult = posteriorOptimiser.findMaximumLogPdf(analyticPosterior.mean).unsafeRunSync()

    println(
      s"\nSetting up sampling..."
    )

    val posteriorSampler = HmcmcPosteriorSampler(
      hmcmcConfig = kenConfig.inferenceParameters.hmcmcParameters,
      posterior = posterior,
      sampleRequestSetCallback = samplesRequestsCount =>
        print(
          s"\rSample requested: Current requests queued: $samplesRequestsCount"
        ),
      sampleRequestUpdateCallback = samplesRequestsCount =>
        print(
          s"\rSample acquired: Current requests queued: $samplesRequestsCount"
        ),
      seedSpec = IO(optimisationResult._2)
    )

    println(
      s"\nInitialising sampler..."
    )

    posteriorSampler.init.unsafeRunSync()

    println(
      s"Inference setup is complete..."
    )

    val samples: List[List[Double]] =
      (0 until kenConfig.visualisationParameters.sampleCount).toList.parTraverse { _ =>
        posteriorSampler.sample.map(_.get("mass-per-length-parametrization"))
      }.map(_.flatten.map(_.toList)).unsafeRunSync()

    println(
      s"\nSampling is complete..."
    )

    // Prep for creating visualisations and outputs
    val sampleGraph = CartesianSurface(
      UniformAbscissa(0d,
                      bladeGeometry.arcLengthUpperBound,
                      kenConfig.visualisationParameters.numXPlotPoints
      ).abscissaPoints.toVector,
      UniformAbscissa(0, 20, kenConfig.visualisationParameters.numYPlotPoints).abscissaPoints.toVector,
      progressBar.set,
      _ => progressBar.increment(),
      _ => progressBar.finish()
    )

    val csvHeadersForBladeGraphs = List(
      "arclength",
      "normalised arclength",
      "mune position",
      "mune curvature",
      "blade haba",
      "blade kasane",
      "shinogi haba",
      "mass-per-length inferred mean"
    )

    val unnormalisedAbscissa = UniformAbscissa(bladeGeometry.bladeBaseArcLength, bladeGeometry.arcLengthUpperBound, 200)
    val normalisedAbsciaa    = UniformAbscissa(0, 1.0, 200)

    val inferredMassMapping = RealValuedFunction.linearInterpolation {
      bladeGeometry.taggedMassInferencePolesAndWeights
        .map(_.pole)
        .zip(optimisationResult._2("mass-per-length-parametrization"))
        .map(i => Point2D(i._1, i._2))
    }

    val graphMeasurements: List[List[String]] =
      List(
        unnormalisedAbscissa.abscissaPoints.map(_.toString),
        normalisedAbsciaa.abscissaPoints.map(_.toString),
        unnormalisedAbscissa.abscissaPoints.map(bladeGeometry.arcLengthToMune.evalAt).map(_.toString),
        unnormalisedAbscissa.abscissaPoints.map(bladeGeometry.arcLengthToMuneCurvature.evalAt).map(_.toString),
        unnormalisedAbscissa.abscissaPoints.map(bladeGeometry.arcLengthToBladeHaba.evalAt).map(_.toString),
        unnormalisedAbscissa.abscissaPoints.map(bladeGeometry.arcLengthToBladeKasane.evalAt).map(_.toString),
        unnormalisedAbscissa.abscissaPoints.map(bladeGeometry.arcLengthToBladeShinogi.evalAt).map(_.toString),
        unnormalisedAbscissa.abscissaPoints.map(inferredMassMapping.evalAt).map(_.toString)
      ).transpose

    val csvHeadersForScalars = List(
      "Scalar Name",
      "Value"
    )

    val scalarMeasurements: List[List[String]] =
      List(
        List("Nagasa", bladeGeometry.nagasa.toString),
        List("Sori", bladeGeometry.sori.toString),
        List("Total Arc Length", bladeGeometry.arcLengthUpperBound.toString)
      )

    val writePlotData =
      for {
        maxLogPdf <- IO(optimisationResult._1)
        _         <- IO(println(s"MaxLogPdf - $maxLogPdf"))
        _         <- IO(println(s"Writing scalar data - ${kenConfig.label}"))
        _ <-
          IO(
            DataWriter.writeCsvFile(
              csvHeadersForScalars,
              scalarMeasurements,
              (resultPath / "scalar_data.csv").toString()
            )
          )
        _ <- IO(println(s"Writing graph data - ${kenConfig.label}"))
        _ <-
          IO(
            DataWriter.writeCsvFile(
              csvHeadersForBladeGraphs,
              graphMeasurements,
              (resultPath / "graph_data.csv").toString()
            )
          )
        _ <-
          IO(
            println(
              s"Creating mass density sample visualisation - ${kenConfig.label}"
            )
          )
        _ <- sampleGraph.addSamples(
               bladeGeometry.taggedMassInferencePolesAndWeights.map(_.pole).toVector,
               samples.map(_.toVector).toVector,
               kenConfig.visualisationParameters.graphLineIncrement,
               kenConfig.visualisationParameters.graphKernelVariance
             )
        results <- sampleGraph.getResults
        _       <- IO(dataWriter.writeDatFile(results.toList, (resultPath / "massDensity_analytic.dat").toString))
        _       <- IO(println(s"\nDone - ${kenConfig.label}!"))
      } yield ()

    println(
      s"\nWriting plot data..."
    )

    writePlotData.unsafeRunSync()
  }
}
