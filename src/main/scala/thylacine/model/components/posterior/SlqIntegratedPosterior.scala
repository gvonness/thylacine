package ai.entrolution
package thylacine.model.components.posterior

import thylacine.config.SlqConfig
import thylacine.model.components.likelihood._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._
import thylacine.model.integration.slq.SlqEngine
import thylacine.model.support.SlqTelemetryUpdate

case class SlqIntegratedPosterior(
    slqConfig: SlqConfig,
    priors: Set[Prior[_]],
    likelihoods: Set[Likelihood[_, _]],
    seeds: Set[ModelParameterCollection] = Set(),
    slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit
) extends NonAnalyticPosterior(priors, likelihoods)
    with SlqEngine[Prior[_], Likelihood[_, _]] {
  override protected final val slqSamplePoolSize: Int = slqConfig.poolSize

  override protected final val slqNumberOfAbscissa: Int =
    slqConfig.abscissaNumber

  override protected final val slqScalingIncrement: Double =
    slqConfig.domainScalingIncrement

  override protected final val slqNominalAcceptanceRatio: Double =
    slqConfig.targetAcceptanceProbability

  override protected final val slqSampleParallelism: Int =
    slqConfig.sampleParallelism

  override def sampleModelParameters: ResultOrErrIo[ModelParameterCollection] =
    getSimulatedSample

  override protected def rawSampleModelParameters
      : ResultOrErrIo[VectorContainer] =
    for {
      sample <- sampleModelParameters
      result <- modelParameterCollectionToRawVector(sample)
    } yield VectorContainer(result)
}
