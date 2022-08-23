package ai.entrolution
package thylacine.model.components.posterior

import bengal.stm.STM
import thylacine.config.HmcmcConfig
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.sampling.hmcmc.HmcmcEngine

import cats.effect.IO

case class HmcmcPosteriorSampler(
    hmcmcConfig: HmcmcConfig,
    posterior: NonAnalyticPosterior,
    sampleRequestSetCallback: Int => Unit = _ => (),
    sampleRequestUpdateCallback: Int => Unit = _ => (),
    epsilonUpdateCallback: Double => Unit = _ => (),
    dhMonitorCallback: Double => Unit = _ => (),
    seedSpec: IO[Map[String, Vector[Double]]] = IO.pure(Map())
)(implicit stm: STM[IO])
    extends HmcmcEngine {

  override protected final val sampleParallelism: Int =
    hmcmcConfig.sampleParallelism.getOrElse(
      Math
        .max(Math.ceil(Runtime.getRuntime.availableProcessors() / 2.0), 1)
        .toInt
    )

  override protected final val simulationsBetweenSamples: Int =
    hmcmcConfig.stepsBetweenSamples

  override protected final val stepsInSimulation: Int =
    hmcmcConfig.stepsInDynamicsSimulation

  override protected final val simulationInitialEpsilon: Double =
    hmcmcConfig.dynamicsSimulationStepSize

  override protected final val maxEpsilonHistory: Int =
    hmcmcConfig.maxStepSizeHistoryForAdjustment

  override protected final val targetAcceptance: Double =
    hmcmcConfig.targetAcceptanceRatio

  override protected final val warmUpSimulationCount: Int =
    hmcmcConfig.warmupStepCount

  override protected final val startingPoint: ResultOrErrIo[ModelParameterCollection] =
    ResultOrErrIo.fromIo(seedSpec.map(IndexedVectorCollection(_)))

  def init: IO[Unit] =
    for {
      _ <- initialise
      _ <- waitForInitialisation
    } yield ()
}
