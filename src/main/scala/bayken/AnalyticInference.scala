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
import bayken.model.ken.{ShinkenGeometry, ShinkenSectionLabel}
import bayken.model.measurement.{BalanceBeamExperimentProcessor, MeasurementRow}
import bayken.util.{DataWriter, ProgressBarOps}
import thylacine.model.components.likelihood.GaussianLinearLikelihood
import thylacine.model.components.posterior.GaussianAnalyticPosterior
import thylacine.model.components.prior.GaussianPrior
import thylacine.visualisation.CartesianSurface

import ai.entrolution.bayken.model.ken.ShinkenSectionLabel.{Habaki, Tsuba, Tsuka}
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.IORuntime
import cats.implicits._
import os.Path

object AnalyticInference {

  def runAndVisualize(
      kenConfig: ShinkenConfig,
      resultPath: Path
  )(implicit ior: IORuntime): Unit = {
    val progressBar: ProgressBarOps.type = ProgressBarOps
    val dataWriter: DataWriter.type      = DataWriter

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
        values = bladeGeometry.taggedMassInferencePolesAndWeights.toVector.map { bsc =>
          bsc.label match {
            case Tsuba  => 200
            case Habaki => 10
            case Tsuka  => 2
            case _      => 0.2 * kenConfig.measurements.shinkenMass / bladeGeometry.arcLengthUpperBound
          }
        },
        covarianceMatrix = bladeGeometry.gaussianProcessCovariance
      )

    // ------------- -------- ----- --- -- - -
    // Likelihood Construction
    // - - -- --- ----- -------- -------------

    val experimentProcessor: BalanceBeamExperimentProcessor =
      BalanceBeamExperimentProcessor(bladeGeometry)

    def getLikelihood(
        measurements: Vector[MeasurementRow]
    ): GaussianLinearLikelihood =
      GaussianLinearLikelihood(
        coefficients = measurements.map(_.rowCoefficients),
        measurements = measurements.map(_.solveConstant),
        uncertainties = measurements.map(_.uncertainty),
        prior = massPerLengthPrior
      )

    val totalMassLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        Vector(
          experimentProcessor.totalMassMeasurement(kenConfig.measurements.shinkenMass,
                                                   kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val zeroMassAtTipLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        Vector(
          experimentProcessor.zeroMassAtTipMeasurement(
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val massContinuousAtKissakeLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        Vector(
          experimentProcessor.massContinuousAroundKissake(
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )
      }

    val balanceBeamMeasurementsLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        kenConfig.measurements.measurementModels.flatMap { mm =>
          experimentProcessor.processExperiment(mm)
        }.toVector
      }

    // ------------- -------- ----- --- -- - -
    // Posterior Construction
    // - - -- --- ----- -------- -------------

    val posterior = GaussianAnalyticPosterior(
      priors = Set(massPerLengthPrior),
      likelihoods = Set(
        totalMassLikelihood,
        zeroMassAtTipLikelihood,
        massContinuousAtKissakeLikelihood,
        balanceBeamMeasurementsLikelihood
      )
    )

    println(
      s"Inference setup is complete..."
    )

    val evaluationAbscissa = UniformAbscissa(
      0d,
      bladeGeometry.arcLengthUpperBound,
      100
    )

    val samples: List[List[Double]] =
      (0 until kenConfig.visualisationParameters.sampleCount).toList.parTraverse { _ =>
        for {
          modelParams <- posterior.sample
        } yield modelParams
          .get(massPerLengthPrior.label)
          .map(_.toList)
      }.map(_.flatten).unsafeRunSync()

    // Prep for creating visualisations and outputs
    val sampleGraph = CartesianSurface(
      UniformAbscissa(0d,
                      bladeGeometry.arcLengthUpperBound,
                      kenConfig.visualisationParameters.numXPlotPoints
      ).abscissaPoints.toVector,
      UniformAbscissa(0, 50, kenConfig.visualisationParameters.numYPlotPoints).abscissaPoints.toVector,
      progressBar.set,
      _ => progressBar.increment(),
      _ => progressBar.finish()
    )

    val writePlotData =
      for {
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

    writePlotData.unsafeRunSync()
  }
}
