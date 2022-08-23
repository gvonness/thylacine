package ai.entrolution
package thylacine.model.components.posterior

import bengal.stm.STM
import thylacine.config.SlqConfig
import thylacine.model.components.prior.Prior
import thylacine.model.integration.slq.{SlqEngine, SlqTelemetryUpdate}

import cats.effect.IO

case class SlqIntegratedPosterior(
    slqConfig: SlqConfig,
    posterior: Posterior[Prior[_], _],
    seeds: Set[ModelParameterCollection] = Set(),
    slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit = _ => (),
    domainRebuildStartCallback: Unit => Unit = _ => (),
    domainRebuildFinishCallback: Unit => Unit = _ => ()
)(implicit stm: STM[IO])
    extends SlqEngine {
  override protected final val slqSamplePoolSize: Int = slqConfig.poolSize

  override protected final val slqNumberOfAbscissa: Int =
    slqConfig.abscissaNumber

  override protected final val slqScalingIncrement: Double =
    slqConfig.domainScalingIncrement

  override protected final val slqNominalAcceptanceRatio: Double =
    slqConfig.targetAcceptanceProbability

  override protected final val slqSampleParallelism: Int =
    slqConfig.sampleParallelism

}
