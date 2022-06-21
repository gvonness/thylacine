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
import bayken.model.ken.BladeGeometry
import bayken.model.measurement.{BalanceBeamExperimentProcessor, MeasurementRow}
import thylacine.model.components.forwardmodel.NonLinearFiniteDifferenceInMemoryMemoizedForwardModel
import thylacine.model.components.likelihood.GaussianLikelihood
import thylacine.model.components.posterior.UnNormalisedPosterior
import thylacine.model.components.prior.{GaussianPrior, UniformPrior}
import thylacine.model.optimization.HookeAndJeevesOptimisedPosterior
import thylacine.model.sampling.hmcmc.HmcmcPosteriorSampler

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.implicits._
import os.Path

object CombinedInference {

  def runAndVisualize(
      kenConfig: KenConfig,
      resultPath: Path
  )(implicit ior: IORuntime): Unit = {
    println(
      s"Performing non-analytic inference of mass density profile - ${kenConfig.label}"
    )

    println(
      s"Setting up inference..."
    )

    val data: KenMeasurements               = kenConfig.measurements
    val rawMunePoints: Seq[MunePointConfig] = data.components.blade.geometry
    val uncertainties: MeasurementUncertaintiesConfig =
      data.measurementUncertainties

    // ------------- -------- ----- --- -- - -
    // Prior Construction
    // - - -- --- ----- -------- -------------

    val xPointMeasurementPrior: Option[GaussianPrior] =
      uncertainties.munePointXUncertainty.map { xUncertainty =>
        GaussianPrior(
          label = "mune-x-uncertainty",
          values = rawMunePoints.map(_.x).toVector,
          confidenceIntervals =
            List.fill(rawMunePoints.size)(xUncertainty).toVector
        )
      }

    val yPointMeasurementPrior: Option[GaussianPrior] =
      uncertainties.munePointYUncertainty.map { yUncertainty =>
        GaussianPrior(
          label = "mune-y-uncertainty",
          values = rawMunePoints.map(_.y).toVector,
          confidenceIntervals =
            List.fill(rawMunePoints.size)(yUncertainty).toVector
        )
      }

    val bladeWidthPrior: Option[GaussianPrior] =
      uncertainties.bladeWidthUncertainty.map { widthUncertainty =>
        val widths: Seq[Double] = rawMunePoints.flatMap(_.width)

        GaussianPrior(
          label = "blade-width-uncertainty",
          values = widths.toVector,
          confidenceIntervals =
            List.fill(widths.size)(widthUncertainty).toVector
        )
      }

    // We use a maximum entropy prior with the constraints of
    // mass-per-length is a physical quantity (i.e. non-negative)
    // and taking into account that Tsuba can spike this value very
    // high (can be 170g+ in .5cm for heavy Tsuba).
    // More information on the importance of this prior can be found
    // in the write-up in the `docs` directory
    val massPerLengthPrior: UniformPrior =
      UniformPrior(
        label = "mass-per-length",
        minBounds = Vector.fill(kenConfig.massInferenceOrder)(0d),
        maxBounds = Vector.fill(kenConfig.massInferenceOrder)(500d)
      )

    // ------------- -------- ----- --- -- - -
    // Likelihood Construction
    // - - -- --- ----- -------- -------------

    def forwardModelCoreMapping(
        input: Map[String, Vector[Double]]
    ): Vector[Double] = {
      val experimentProcessor: BalanceBeamExperimentProcessor =
        BalanceBeamExperimentProcessor(BladeGeometry(
                                         rawMunePoints = rawMunePoints.toList,
                                         munePointXOverride =
                                           xPointMeasurementPrior.flatMap(p =>
                                             input
                                               .get(p.label)
                                               .map(_.toList)
                                           ),
                                         munePointYOverride =
                                           yPointMeasurementPrior.flatMap(p =>
                                             input
                                               .get(p.label)
                                               .map(_.toList)
                                           ),
                                         bladeWidthOverride =
                                           bladeWidthPrior.flatMap(p =>
                                             input
                                               .get(p.label)
                                               .map(_.toList)
                                           )
                                       ),
                                       kenConfig.inferenceParameters
        )

      val measurementRows: Seq[MeasurementRow] =
        kenConfig.measurements.measurementModels.flatMap { mm =>
          experimentProcessor.processExperiment(mm)
        } ++ Seq(
          experimentProcessor.totalMassMeasurement(
            kenConfig.measurements.kenMass,
            kenConfig.measurements.measurementUncertainties.massUncertainty
          )
        )

      (DenseMatrix(
        measurementRows.toList.map(_.rowCoefficients.toArray): _*
      ) * DenseVector(
        input(massPerLengthPrior.label).toArray
      )).toScalaVector
    }

    val collectedPriors = List(
      xPointMeasurementPrior,
      yPointMeasurementPrior,
      bladeWidthPrior,
      Some(massPerLengthPrior)
    ).flatten

    val forwardModelDomain: Map[String, Int] =
      collectedPriors.map(gp => gp.label -> gp.domainDimension).toMap

    val balanceBeamExperimentLikelihood: GaussianLikelihood[
      NonLinearFiniteDifferenceInMemoryMemoizedForwardModel
    ] =
      GaussianLikelihood[NonLinearFiniteDifferenceInMemoryMemoizedForwardModel](
        NonLinearFiniteDifferenceInMemoryMemoizedForwardModel(
          forwardModelCoreMapping,
          forwardModelDomain,
          kenConfig.measurements.measurementModels.size * 2 + 1,
          .00000001,
          forwardModelDomain.values.sum * 2
        ),
        kenConfig.measurements.measurementModels.flatMap(mm =>
          Seq(mm.solveConstant, mm.dualSolveConstant)
        ) ++ Seq(kenConfig.measurements.kenMass),
        kenConfig.measurements.measurementModels.flatMap(mm =>
          Seq(mm.uncertainty, mm.dualUncertainty)
        ) ++ Seq(
          kenConfig.measurements.measurementUncertainties.massUncertainty
        )
      )

    // ------------- -------- ----- --- -- - -
    // Posterior Construction
    // - - -- --- ----- -------- -------------

    val posterior = UnNormalisedPosterior(
      priors = collectedPriors.toSet,
      likelihoods = Set(balanceBeamExperimentLikelihood)
    )

    // ------------- -------- ----- --- -- - -
    // Posterior Optimisation
    // We find a local (hopefully near the
    // global) maxima to give our HMCMC
    // sampler a decent place to start
    // - - -- --- ----- -------- -------------

    val posteriorOptimiser: HookeAndJeevesOptimisedPosterior =
      HookeAndJeevesOptimisedPosterior(
        hookeAndJeevesConfig =
          kenConfig.inferenceParameters.hookesAndJeevesParams,
        posterior = posterior,
        newMaximumCallback = nm =>
          println(
            s"Hooke & Jeeves Optimisation: New posterior maximum found of $nm"
          ),
        newScaleCallback =
          ns => println(s"Hooke & Jeeves Optimisation: Rescaling to $ns"),
        isConvergedCallback =
          _ => println(s"Hooke & Jeeves Optimisation: Has converged!")
      )

    val posteriorSampler = HmcmcPosteriorSampler(
      hmcmcConfig = kenConfig.inferenceParameters.hmcmcParams,
      posterior = posterior,
      sampleRequestSetCallback = samplesRequestsCount =>
        println(
          s"Sample requested: Current requests queued: $samplesRequestsCount"
        ),
      sampleRequestUpdateCallback = samplesRequestsCount =>
        println(
          s"Sample acquired: Current requests queued: $samplesRequestsCount"
        ),
      seedSpec = posteriorOptimiser.findMaximumLogPdf.map(_._2)
    )

    println(
      s"Inference setup is complete..."
    )

    val samples: IO[List[Map[String, Vector[Double]]]] =
      (0 until kenConfig.visualisationParameters.sampleCount).toList.parTraverse {
        _ =>
          posteriorSampler.sample
      }

    // If we have not recorded uncertainties in geometry measurements, there
    // is no need to recalculate the geometry for the samples
    lazy val baseExperimentProcessor =
      BalanceBeamExperimentProcessor(
        BladeGeometry(rawMunePoints = rawMunePoints.toList, None, None, None),
        kenConfig.inferenceParameters
      )

    // For each sample, we need to get the coordinate system
    def getGeometry(
        sample: Map[String, Vector[Double]]
    ): BalanceBeamExperimentProcessor =
      if (
        uncertainties.munePointXUncertainty.isDefined || uncertainties.munePointYUncertainty.isDefined || uncertainties.bladeWidthUncertainty.isDefined
      ) {
        BalanceBeamExperimentProcessor(BladeGeometry(
                                         rawMunePoints = rawMunePoints.toList,
                                         munePointXOverride =
                                           xPointMeasurementPrior.flatMap(p =>
                                             sample.get(p.label).map(_.toList)
                                           ),
                                         munePointYOverride =
                                           yPointMeasurementPrior.flatMap(p =>
                                             sample.get(p.label).map(_.toList)
                                           ),
                                         bladeWidthOverride =
                                           bladeWidthPrior.flatMap(p =>
                                             sample.get(p.label).map(_.toList)
                                           )
                                       ),
                                       kenConfig.inferenceParameters
        )
      } else {
        baseExperimentProcessor
      }

    println(
      s"Sample generation complete - ${kenConfig.label}"
    )

    // Need to calculate
    // Mass-per-length vs arc-length: Inferred (need experiment processor)
    // Mass-per-length vs linear x: Can calculate from above and arc-length mapping in blade geometry
    //    (Is this helpful, as it depends on a particular rotation of the blade?)
    // Mune curvature vs arc-length: Can pull mapping from blade geometry
    // Blade width vs arc-length: Can pull mapping from blade geometry
    // Tang/Tsuba points vs arc-length: Can pull from blade geometry
    // Kissake point vs arc-length: Can pull from blade geometry
    // Chordal length: Can get from blade geometry and is probably best representation of sword "length"
    // Sori: Traditional measurement of katana curvature

    //    val valueStats: List[ValueStatistics] =
    //      samples.map(ValueStatistics(legendreQuadrature, 0, data.kenLength, _))
    //
    //    val sampleGraph = CartesianSurface(
    //      UniformAbscissa(visualisationParams.xAbscissa.min,
    //                      visualisationParams.xAbscissa.max,
    //                      visualisationParams.xAbscissa.size
    //      ).abscissaPoints.toVector,
    //      UniformAbscissa(visualisationParams.yAbscissa.min,
    //                      visualisationParams.yAbscissa.max,
    //                      visualisationParams.yAbscissa.size
    //      ).abscissaPoints.toVector,
    //      ProgressBarOps.set,
    //      _ => ProgressBarOps.increment(),
    //      _ => ProgressBarOps.increment()
    //    )
    //
    //    val cdfGraph = CartesianSurface(
    //      UniformAbscissa(visualisationParams.xAbscissa.min,
    //                      visualisationParams.xAbscissa.max,
    //                      visualisationParams.xAbscissa.size
    //      ).abscissaPoints.toVector,
    //      UniformAbscissa(0,
    //                      1,
    //                      visualisationParams.yAbscissa.size
    //      ).abscissaPoints.toVector,
    //      ProgressBarOps.set,
    //      _ => ProgressBarOps.increment(),
    //      _ => ProgressBarOps.increment()
    //    )
    //
    //    val csvHeaders = List(
    //      "Statistics",
    //      "Mean",
    //      "Lower Confidence (95%)",
    //      "Upper Confidence (95%)"
    //    )
    //
    //    // Accumulate sample statistics
    //    val medianStats = {
    //      val confidenceInterval =
    //        MathOps.confidenceInterval95(valueStats.map(_.median))
    //      List(
    //        "median",
    //        MathOps.mean(valueStats.map(_.median)).toString,
    //        confidenceInterval._1.toString,
    //        confidenceInterval._2.toString
    //      )
    //    }
    //
    //    val meanStats = {
    //      val confidenceInterval =
    //        MathOps.confidenceInterval95(valueStats.map(_.integratedMean))
    //      List(
    //        "integrated mean",
    //        MathOps.mean(valueStats.map(_.integratedMean)).toString,
    //        confidenceInterval._1.toString,
    //        confidenceInterval._2.toString
    //      )
    //    }
    //
    //    val stdStats = {
    //      val confidenceInterval =
    //        MathOps.confidenceInterval95(valueStats.map(_.standardDeviation))
    //      List(
    //        "std",
    //        MathOps.mean(valueStats.map(_.standardDeviation)).toString,
    //        confidenceInterval._1.toString,
    //        confidenceInterval._2.toString
    //      )
    //    }
    //
    //    val thirdMomentStats = {
    //      val confidenceInterval = MathOps.confidenceInterval95(
    //        valueStats.map(_.centralNormalisedMoment(3))
    //      )
    //      List(
    //        "3rd Norm. Moment",
    //        MathOps.mean(valueStats.map(_.centralNormalisedMoment(3))).toString,
    //        confidenceInterval._1.toString,
    //        confidenceInterval._2.toString
    //      )
    //    }
    //
    //    val fourthMomentStats = {
    //      val confidenceInterval = MathOps.confidenceInterval95(
    //        valueStats.map(_.centralNormalisedMoment(4))
    //      )
    //      List(
    //        "4th Norm. Moment",
    //        MathOps.mean(valueStats.map(_.centralNormalisedMoment(4))).toString,
    //        confidenceInterval._1.toString,
    //        confidenceInterval._2.toString
    //      )
    //    }
    //
    //    val fifthMomentStats = {
    //      val confidenceInterval = MathOps.confidenceInterval95(
    //        valueStats.map(_.centralNormalisedMoment(5))
    //      )
    //      List(
    //        "5th Norm. Moment",
    //        MathOps.mean(valueStats.map(_.centralNormalisedMoment(5))).toString,
    //        confidenceInterval._1.toString,
    //        confidenceInterval._2.toString
    //      )
    //    }
    //
    //    val allStats = List(
    //      medianStats,
    //      meanStats,
    //      stdStats,
    //      thirdMomentStats,
    //      fourthMomentStats,
    //      fifthMomentStats
    //    )
    //
    //    val writePlotData =
    //      for {
    //        _ <- IO(println(s"Writing value statistics - ${kenConfig.label}"))
    //        _ <-
    //          IO(
    //            DataWriter.writeCsvFile(csvHeaders,
    //                                    allStats,
    //                                    (resultPath / "sampleStats.csv").toString()
    //            )
    //          )
    //        _ <-
    //          IO(
    //            println(
    //              s"Creating mass density sample visualisation - ${kenConfig.label}"
    //            )
    //          )
    //        _ <- sampleGraph.addSamples(
    //               LegendreAbscissa(legendreQuadrature).abscissaPoints.toVector,
    //               samples.map(_.toVector).toVector,
    //               visualisationParams.graphLineIncrement,
    //               visualisationParams.graphKernelVariance
    //             )
    //        _ <- sampleGraph.getResults.map(i =>
    //               writeDatFile(i.toList, (resultPath / "massDensity.dat").toString)
    //             )
    //        _ <-
    //          IO(
    //            println(
    //              s"\nCreating mass density CDF visualisation - ${kenConfig.label}"
    //            )
    //          )
    //        _ <- cdfGraph.addSamples(
    //               LegendreAbscissa(legendreQuadrature).abscissaPoints.toVector,
    //               valueStats.map(_.normalisedCdf.toVector).toVector,
    //               visualisationParams.graphLineIncrement,
    //               visualisationParams.graphKernelVariance
    //             )
    //        _ <-
    //          cdfGraph.getResults.map(i =>
    //            writeDatFile(i.toList, (resultPath / "massDensityCDF.dat").toString)
    //          )
    //        _ <- IO(println(s"\nDone - ${kenConfig.label}!"))
    //      } yield ()
    //
    //    writePlotData.unsafeRunSync()
    //  }
    //
    //  def getIntegrationCoefficients(
    //      quadrature: LegendreQuadrature,
    //      lowerBound: Double,
    //      upperBound: Double,
    //      balancePointPosition: Double
    //  ): List[Double] =
    //    quadrature.getPolesAndWeights(lowerBound, upperBound).map { i =>
    //      i._2 * (i._1 - balancePointPosition)
    //    }

  }
}
