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

//import bayken.config._
//import bayken.config.measurements._
//import bayken.model.ken.{ShinkenGeometry, ShinkenSectionLabel}
//import bayken.model.measurement.{BalanceBeamExperimentProcessor, MeasurementRow}
//import thylacine.model.components.forwardmodel.NonLinearFiniteDifferenceInMemoryMemoizedForwardModel
//import thylacine.model.components.likelihood.GaussianLikelihood
//import thylacine.model.components.posterior.UnNormalisedPosterior
//import thylacine.model.components.prior.{GaussianPrior, UniformPrior}
//import thylacine.model.optimization.HookeAndJeevesOptimisedPosterior
//import thylacine.model.sampling.hmcmc.HmcmcPosteriorSampler
//
//import cats.Parallel
//import cats.effect.IO
//import cats.effect.implicits._
//import cats.effect.unsafe.IORuntime
//import cats.implicits._
//import os.Path

object CombinedInference {

//  def runAndVisualize(
//      kenConfig: ShinkenConfig,
//      resultPath: Path
//  )(implicit ior: IORuntime): Unit = {
//    println(
//      s"Performing non-analytic inference of mass density profile - ${kenConfig.label}"
//    )
//
//    println(
//      s"Setting up inference..."
//    )
//
//    val data: ShinkenMeasurements               = kenConfig.measurements
//    val rawMunePoints: Seq[BackEdgePointConfig] = data.pointMeasurements
//    val uncertainties: MeasurementUncertaintiesConfig =
//      data.measurementUncertainties
//    val measuredSections: Seq[ShinkenSectionLabel] =
//      rawMunePoints.map(_.shinkenSection).distinct
//    val massPerLengthParametrisationDimension: Int = measuredSections.flatMap { bSec =>
//      kenConfig.inferenceParameters.bladeSectionParameters
//        .find(sec => bSec == sec.label)
//        .map(_.massPolynomialFitOrder)
//    }.sum
//
//    // ------------- -------- ----- --- -- - -
//    // Prior Construction
//    // - - -- --- ----- -------- -------------
//
//    val xPointMeasurementPrior: Option[GaussianPrior] =
//      uncertainties.munePointXUncertainty.map { xUncertainty =>
//        GaussianPrior(
//          label = "mune-x-uncertainty",
//          values = rawMunePoints.map(_.x).toVector,
//          confidenceIntervals = List.fill(rawMunePoints.size)(xUncertainty).toVector
//        )
//      }
//
//    val yPointMeasurementPrior: Option[GaussianPrior] =
//      uncertainties.munePointYUncertainty.map { yUncertainty =>
//        GaussianPrior(
//          label = "mune-y-uncertainty",
//          values = rawMunePoints.map(_.y).toVector,
//          confidenceIntervals = List.fill(rawMunePoints.size)(yUncertainty).toVector
//        )
//      }
//
//    val bladeWidthPrior: Option[GaussianPrior] =
//      uncertainties.bladeWidthUncertainty.map { widthUncertainty =>
//        val widths: Seq[Double] = rawMunePoints.flatMap(_.haba)
//
//        GaussianPrior(
//          label = "blade-width-uncertainty",
//          values = widths.toVector,
//          confidenceIntervals = List.fill(widths.size)(widthUncertainty).toVector
//        )
//      }
//
//    val useStaticGeometry: Boolean =
//      xPointMeasurementPrior.isEmpty && yPointMeasurementPrior.isEmpty && bladeWidthPrior.isEmpty
//
//    // We parameterise mass-per-length as piecewise polynomial coefficients
//    // corresponding to the measured geometry of the blade. This model enables
//    // us to account for expected jumps in mass density (due to the sword be
//    // constructed of heteroenous parts) while suppressing high-frequency, deconvolution
//    // noise. We use an 'effective' max-entropy (uniform) prior with bounds
//    // set to easily encompass any physically-realistic parametrisation
//    val massPerLengthPrior: UniformPrior =
//      UniformPrior(
//        label = "mass-per-length-parametrization",
//        minBounds = Vector.fill(massPerLengthParametrisationDimension)(-10000),
//        maxBounds = Vector.fill(massPerLengthParametrisationDimension)(10000)
//      )
//
//    // ------------- -------- ----- --- -- - -
//    // Likelihood Construction
//    // - - -- --- ----- -------- -------------
//
//    // While we generally try to avoid IO on this layer to keep the code
//    // as readable as possible (given the number of mathematical
//    // manipulations), here there is a canonical point to parallelise
//    // across experiment measurements, as each requires a new set
//    // of blade geometry calculations.
//
//    def getMeasurementRows(
//        input: Map[String, Vector[Double]]
//    ): (BalanceBeamExperimentProcessor, IO[Seq[MeasurementRow]]) = {
//      lazy val bladeGeometry: ShinkenGeometry = ShinkenGeometry(
//        rawMunePoints = rawMunePoints.toList,
//        inferenceConfig = kenConfig.inferenceParameters,
//        munePointXOverride = xPointMeasurementPrior.flatMap(p =>
//          input
//            .get(p.label)
//            .map(_.toList)
//        ),
//        munePointYOverride = yPointMeasurementPrior.flatMap(p =>
//          input
//            .get(p.label)
//            .map(_.toList)
//        ),
//        habaOverride = bladeWidthPrior.flatMap(p =>
//          input
//            .get(p.label)
//            .map(_.toList)
//        )
//      )
//
//      lazy val experimentProcessor: BalanceBeamExperimentProcessor =
//        BalanceBeamExperimentProcessor(bladeGeometry)
//
//      lazy val rows: IO[Seq[MeasurementRow]] =
//        Parallel
//          .parSequence(
//            kenConfig.measurements.measurementModels.map { mm =>
//              IO(experimentProcessor.processExperiment(mm))
//            } ++ Seq(
//              IO(
//                Seq(
//                  experimentProcessor.totalMassMeasurement(
//                    kenConfig.measurements.shinkenMass,
//                    kenConfig.measurements.measurementUncertainties.massUncertainty
//                  )
//                )
//              ),
//              IO(
//                Seq(
//                  experimentProcessor.zeroMassAtTipMeasurement(
//                    kenConfig.measurements.measurementUncertainties.massUncertainty
//                  )
//                )
//              ),
//              IO(
//                Seq(
//                  experimentProcessor.massContinuousAroundKissake(
//                    kenConfig.measurements.measurementUncertainties.massUncertainty
//                  )
//                )
//              )
//            )
//          )
//          .map(_.flatten)
//
//      (experimentProcessor, rows)
//    }
//
//    lazy val (staticExperiementProcessor: BalanceBeamExperimentProcessor,
//              staticMeasurementRows: IO[Seq[MeasurementRow]]
//    ) =
//      getMeasurementRows(Map())
//
//    def forwardModelCoreMapping(
//        input: Map[String, Vector[Double]]
//    ): Vector[Double] = {
//      val massCoefficients = input(massPerLengthPrior.label)
//
//      val resultIo = for {
//        rows <- if (useStaticGeometry) {
//                  staticMeasurementRows
//                } else {
//                  getMeasurementRows(input)._2
//                }
//        result <- rows.parTraverse(r =>
//                    IO(r.massCoefficientMapping(massCoefficients)).map(res =>
//                      r.rowCoefficients.zip(res).map(i => i._1 * i._2).sum
//                    )
//                  )
//      } yield result.toVector
//
//      resultIo.unsafeRunSync()
//    }
//
//    val collectedPriors = List(
//      xPointMeasurementPrior,
//      yPointMeasurementPrior,
//      bladeWidthPrior,
//      Some(massPerLengthPrior)
//    ).flatten
//
//    val forwardModelDomain: Map[String, Int] =
//      collectedPriors.map(gp => gp.label -> gp.domainDimension).toMap
//
//    val balanceBeamExperimentLikelihood: GaussianLikelihood[
//      NonLinearFiniteDifferenceInMemoryMemoizedForwardModel
//    ] =
//      GaussianLikelihood[NonLinearFiniteDifferenceInMemoryMemoizedForwardModel](
//        NonLinearFiniteDifferenceInMemoryMemoizedForwardModel(
//          forwardModelCoreMapping,
//          forwardModelDomain,
//          kenConfig.measurements.measurementModels.size * 2 + 1,
//          .00000001,
//          forwardModelDomain.values.sum * 2
//        ),
//        kenConfig.measurements.measurementModels.flatMap(mm => Seq(mm.solveConstant, mm.dualSolveConstant)) ++ Seq(
//          kenConfig.measurements.shinkenMass
//        ),
//        kenConfig.measurements.measurementModels.flatMap(mm => Seq(mm.uncertainty, mm.dualUncertainty)) ++ Seq(
//          kenConfig.measurements.measurementUncertainties.massUncertainty
//        )
//      )
//
//    // ------------- -------- ----- --- -- - -
//    // Posterior Construction
//    // - - -- --- ----- -------- -------------
//
//    val posterior = UnNormalisedPosterior(
//      priors = collectedPriors.toSet,
//      likelihoods = Set(balanceBeamExperimentLikelihood)
//    )
//
//    // ------------- -------- ----- --- -- - -
//    // Posterior Optimisation
//    // We find a local (hopefully near the
//    // global) maxima to give our HMCMC
//    // sampler a decent place to start
//    // - - -- --- ----- -------- -------------
//
//    val posteriorOptimiser: HookeAndJeevesOptimisedPosterior =
//      HookeAndJeevesOptimisedPosterior(
//        hookeAndJeevesConfig = kenConfig.inferenceParameters.hookesAndJeevesParameters,
//        posterior = posterior,
//        newMaximumCallback = nm =>
//          println(
//            s"Hooke & Jeeves Optimisation: New posterior maximum found of $nm"
//          ),
//        newScaleCallback = ns => println(s"Hooke & Jeeves Optimisation: Rescaling to $ns"),
//        isConvergedCallback = _ => println(s"Hooke & Jeeves Optimisation: Has converged!")
//      )
//
//    val posteriorSampler = HmcmcPosteriorSampler(
//      hmcmcConfig = kenConfig.inferenceParameters.hmcmcParameters,
//      posterior = posterior,
//      sampleRequestSetCallback = samplesRequestsCount =>
//        println(
//          s"Sample requested: Current requests queued: $samplesRequestsCount"
//        ),
//      sampleRequestUpdateCallback = samplesRequestsCount =>
//        println(
//          s"Sample acquired: Current requests queued: $samplesRequestsCount"
//        ),
//      seedSpec = posteriorOptimiser.findMaximumLogPdf.map(_._2)
//    )
//
//    println(
//      s"Inference setup is complete..."
//    )
//
//    val samples: IO[List[Map[String, Vector[Double]]]] =
//      (0 until kenConfig.visualisationParameters.sampleCount).toList.parTraverse { _ =>
//        posteriorSampler.sample
//      }
//
//    // For each sample, we need to get the coordinate system
//    // Note that if the geometry isn't part of the sampling
//    // recalculation we fall back to the static geometry
//    def getGeometry(
//        sample: Map[String, Vector[Double]]
//    ): BalanceBeamExperimentProcessor =
//      if (!useStaticGeometry) {
//        BalanceBeamExperimentProcessor(
//          ShinkenGeometry(
//            rawMunePoints = rawMunePoints.toList,
//            inferenceConfig = kenConfig.inferenceParameters,
//            munePointXOverride = xPointMeasurementPrior.flatMap(p => sample.get(p.label).map(_.toList)),
//            munePointYOverride = yPointMeasurementPrior.flatMap(p => sample.get(p.label).map(_.toList)),
//            habaOverride = bladeWidthPrior.flatMap(p => sample.get(p.label).map(_.toList))
//          )
//        )
//      } else {
//        staticExperiementProcessor
//      }
//
//    println(
//      s"Sample generation complete - ${kenConfig.label}"
//    )

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

  //}
}
