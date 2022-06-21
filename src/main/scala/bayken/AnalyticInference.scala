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
import bayken.model.Abscissa.{LegendreAbscissa, UniformAbscissa}
import bayken.model.ValueStatistics
import bayken.model.measurement.{BalanceExperimentMeasurement, MeasurementRow}
import bayken.numerical.LegendreQuadrature
import bayken.util.DataWriter.writeDatFile
import bayken.util._
import thylacine.model.components.likelihood.GaussianLinearLikelihood
import thylacine.model.components.posterior.GaussianAnalyticPosterior
import thylacine.model.components.prior.GaussianPrior
import thylacine.visualisation.CartesianSurface

import ai.entrolution.bayken.config.measurements.{
  KenMeasurements,
  MeasurementUncertaintiesConfig
}
import ai.entrolution.bayken.config.visualisation.MassInferenceVisualisationConfig
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.implicits._
import os.Path

object AnalyticInference {

  def runAndVisualize(
      kenConfig: KenConfig,
      uncertaintiesDefault: => MeasurementUncertaintiesConfig,
      inferenceDefault: => MassInferenceConfig,
      visualisationDefault: => MassInferenceVisualisationConfig,
      resultPath: Path
  )(implicit ior: IORuntime): Unit = {
    println(
      s"Performing inference of mass density profile - ${kenConfig.label}"
    )

    val data: KenMeasurements = kenConfig.measurements
    val uncertainties: MeasurementUncertaintiesConfig =
      kenConfig.measurements.measurementUncertainties

    val inferenceParams: MassInferenceConfig =
      kenConfig.inferenceParameters.getOrElse(inferenceDefault)
    val visualisationParams: MassInferenceVisualisationConfig =
      kenConfig.visualisationParameters.getOrElse(visualisationDefault)

    val priorDimension = inferenceParams.quadratureSize + 1

    val priorValue: Double       = data.kenMass / data.kenLength
    val priorUncertainty: Double = priorValue

    val legendreQuadrature =
      LegendreQuadrature(inferenceParams.quadratureSize + 1)

    val massPerUnitLengthParameterLabel = "Mass per length"

    val massPerLengthPrior =
      GaussianPrior(
        label = massPerUnitLengthParameterLabel,
        values = List.fill(priorDimension)(priorValue).toVector,
        confidenceIntervals =
          List.fill(priorDimension)(priorUncertainty).toVector
      )

    def getLikelihood(
        measurements: List[MeasurementRow]
    ): GaussianLinearLikelihood =
      GaussianLinearLikelihood(
        coefficients = measurements.map(_.rowCoefficients.toVector).toVector,
        measurements = measurements.map(_.solveConstant).toVector,
        uncertainties = measurements.map(_.uncertainty).toVector,
        prior = massPerLengthPrior
      )

    val totalMassLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        List(
          MeasurementRow(
            legendreQuadrature.getPolesAndWeights(0, data.kenLength).map(_._2),
            data.kenMass,
            uncertainties.massUncertaintyMultiplier * data.kenMass + uncertainties.massUncertainty
          )
        )
      }

    val balanceBeamMeasurementLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        data.balanceExperimentMeasurements.map { i =>
          val measurement = BalanceExperimentMeasurement(i, uncertainties, data)

          MeasurementRow(
            getIntegrationCoefficients(
              legendreQuadrature,
              0,
              data.kenLength,
              measurement.fulcrumPosition
            ),
            measurement.solveConstant,
            measurement.uncertainty
          )
        }
      }

    val balanceBeamDualMeasurementLikelihood: GaussianLinearLikelihood =
      getLikelihood {
        data.balanceExperimentMeasurements.map { i =>
          val measurement = BalanceExperimentMeasurement(i, uncertainties, data)

          MeasurementRow(
            getIntegrationCoefficients(
              legendreQuadrature,
              0,
              data.kenLength,
              measurement.counterWeightPosition
            ),
            measurement.dualSolveConstant,
            measurement.dualUncertainty
          )
        }
      }

    val posterior = GaussianAnalyticPosterior(
      priors = Set(massPerLengthPrior),
      likelihoods = Set(totalMassLikelihood,
                        balanceBeamMeasurementLikelihood,
                        balanceBeamDualMeasurementLikelihood
      )
    )

    val samples: List[List[Double]] =
      (0 until visualisationParams.sampleCount).toList.parTraverse { _ =>
        for {
          modelParams <- posterior.sample
        } yield modelParams
          .get(massPerUnitLengthParameterLabel)
          .map(_.toList)
      }.unsafeRunSync().flatten

    println(
      s"Sample generation complete - ${kenConfig.label}"
    )

    val valueStats: List[ValueStatistics] =
      samples.map(ValueStatistics(legendreQuadrature, 0, data.kenLength, _))

    val sampleGraph = CartesianSurface(
      UniformAbscissa(visualisationParams.xAbscissa.min,
                      visualisationParams.xAbscissa.max,
                      visualisationParams.xAbscissa.size
      ).abscissaPoints.toVector,
      UniformAbscissa(visualisationParams.yAbscissa.min,
                      visualisationParams.yAbscissa.max,
                      visualisationParams.yAbscissa.size
      ).abscissaPoints.toVector,
      ProgressBarOps.set,
      _ => ProgressBarOps.increment(),
      _ => ProgressBarOps.increment()
    )

    val cdfGraph = CartesianSurface(
      UniformAbscissa(visualisationParams.xAbscissa.min,
                      visualisationParams.xAbscissa.max,
                      visualisationParams.xAbscissa.size
      ).abscissaPoints.toVector,
      UniformAbscissa(0,
                      1,
                      visualisationParams.yAbscissa.size
      ).abscissaPoints.toVector,
      ProgressBarOps.set,
      _ => ProgressBarOps.increment(),
      _ => ProgressBarOps.increment()
    )

    val csvHeaders = List(
      "Statistics",
      "Mean",
      "Lower Confidence (95%)",
      "Upper Confidence (95%)"
    )

    // Accumulate sample statistics
    val medianStats = {
      val confidenceInterval =
        MathOps.confidenceInterval95(valueStats.map(_.median))
      List(
        "median",
        MathOps.mean(valueStats.map(_.median)).toString,
        confidenceInterval._1.toString,
        confidenceInterval._2.toString
      )
    }

    val meanStats = {
      val confidenceInterval =
        MathOps.confidenceInterval95(valueStats.map(_.integratedMean))
      List(
        "integrated mean",
        MathOps.mean(valueStats.map(_.integratedMean)).toString,
        confidenceInterval._1.toString,
        confidenceInterval._2.toString
      )
    }

    val stdStats = {
      val confidenceInterval =
        MathOps.confidenceInterval95(valueStats.map(_.standardDeviation))
      List(
        "std",
        MathOps.mean(valueStats.map(_.standardDeviation)).toString,
        confidenceInterval._1.toString,
        confidenceInterval._2.toString
      )
    }

    val thirdMomentStats = {
      val confidenceInterval = MathOps.confidenceInterval95(
        valueStats.map(_.centralNormalisedMoment(3))
      )
      List(
        "3rd Norm. Moment",
        MathOps.mean(valueStats.map(_.centralNormalisedMoment(3))).toString,
        confidenceInterval._1.toString,
        confidenceInterval._2.toString
      )
    }

    val fourthMomentStats = {
      val confidenceInterval = MathOps.confidenceInterval95(
        valueStats.map(_.centralNormalisedMoment(4))
      )
      List(
        "4th Norm. Moment",
        MathOps.mean(valueStats.map(_.centralNormalisedMoment(4))).toString,
        confidenceInterval._1.toString,
        confidenceInterval._2.toString
      )
    }

    val fifthMomentStats = {
      val confidenceInterval = MathOps.confidenceInterval95(
        valueStats.map(_.centralNormalisedMoment(5))
      )
      List(
        "5th Norm. Moment",
        MathOps.mean(valueStats.map(_.centralNormalisedMoment(5))).toString,
        confidenceInterval._1.toString,
        confidenceInterval._2.toString
      )
    }

    val allStats = List(
      medianStats,
      meanStats,
      stdStats,
      thirdMomentStats,
      fourthMomentStats,
      fifthMomentStats
    )

    val writePlotData =
      for {
        _ <- IO(println(s"Writing value statistics - ${kenConfig.label}"))
        _ <-
          IO(
            DataWriter.writeCsvFile(csvHeaders,
                                    allStats,
                                    (resultPath / "sampleStats.csv").toString()
            )
          )
        _ <-
          IO(
            println(
              s"Creating mass density sample visualisation - ${kenConfig.label}"
            )
          )
        _ <- sampleGraph.addSamples(
               LegendreAbscissa(legendreQuadrature).abscissaPoints.toVector,
               samples.map(_.toVector).toVector,
               visualisationParams.graphLineIncrement,
               visualisationParams.graphKernelVariance
             )
        _ <- sampleGraph.getResults.map(i =>
               writeDatFile(i.toList, (resultPath / "massDensity.dat").toString)
             )
        _ <-
          IO(
            println(
              s"\nCreating mass density CDF visualisation - ${kenConfig.label}"
            )
          )
        _ <- cdfGraph.addSamples(
               LegendreAbscissa(legendreQuadrature).abscissaPoints.toVector,
               valueStats.map(_.normalisedCdf.toVector).toVector,
               visualisationParams.graphLineIncrement,
               visualisationParams.graphKernelVariance
             )
        _ <-
          cdfGraph.getResults.map(i =>
            writeDatFile(i.toList, (resultPath / "massDensityCDF.dat").toString)
          )
        _ <- IO(println(s"\nDone - ${kenConfig.label}!"))
      } yield ()

    writePlotData.unsafeRunSync()
  }

  def getIntegrationCoefficients(
      quadrature: LegendreQuadrature,
      lowerBound: Double,
      upperBound: Double,
      balancePointPosition: Double
  ): List[Double] =
    quadrature.getPolesAndWeights(lowerBound, upperBound).map { i =>
      i._2 * (i._1 - balancePointPosition)
    }

}
