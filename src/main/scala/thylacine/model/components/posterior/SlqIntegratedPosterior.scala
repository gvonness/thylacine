package ai.entrolution
package thylacine.model.components.posterior

import bengal.stm.STM
import bengal.stm.model.{TxnVar, TxnVarMap}
import thylacine.config.SlqConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.computation.ResultOrErrF.Implicits._
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.integration.slq._

import cats.effect.kernel.Async
import cats.syntax.all._

case class SlqIntegratedPosterior[F[_]: STM: Async](
    slqConfig: SlqConfig,
    slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit = _ => (),
    domainRebuildStartCallback: Unit => Unit = _ => (),
    domainRebuildFinishCallback: Unit => Unit = _ => (),
    seeds: Set[ModelParameterCollection[F]],
    override val priors: Set[Prior[F, _]],
    override val likelihoods: Set[Likelihood[F, _, _]],
    override protected val sampleDomain: TxnVar[F, PointInCubeCollection[F]],
    override protected val samplePool: TxnVarMap[F, Double, ModelParameterCollection[F]],
    override protected val samplePoolMinimumLogPdf: TxnVar[F, Double],
    override protected val logPdfResults: TxnVar[F, Vector[(Double, ModelParameterCollection[F])]],
    override protected val sampleDomainScalingState: TxnVar[F, QuadratureDomainTelemetry],
    override protected val workTokenPool: TxnVar[F, Int],
    override protected val abscissas: TxnVar[F, QuadratureAbscissaCollection],
    override protected val quadratureIntegrations: TxnVar[F, QuadratureIntegrator[F]],
    override protected val samplingSimulation: TxnVar[F, SamplingSimulation[F]],
    override protected val isConverged: TxnVar[F, Boolean]
) extends StmImplicits[F]
    with UnnormalisedPosterior[F]
    with SlqEngine[F] {
  override protected final val slqSamplePoolSize: Int = slqConfig.poolSize

  override protected final val slqNumberOfAbscissa: Int =
    slqConfig.abscissaNumber

  override protected final val slqScalingIncrement: Double =
    slqConfig.domainScalingIncrement

  override protected final val slqNominalAcceptanceRatio: Double =
    slqConfig.targetAcceptanceProbability

  override protected final val slqSampleParallelism: Int =
    slqConfig.sampleParallelism

  def rebuildSampleSimulation: F[Unit] =
    (for {
      _ <- buildSampleSimulation
      _ <- waitForSimulationConstruction
    } yield ()).liftToF
}

object SlqIntegratedPosterior {

  def of[F[_]: STM: Async](
      slqConfig: SlqConfig,
      posterior: Posterior[F, Prior[F, _], Likelihood[F, _, _]],
      slqTelemetryUpdateCallback: SlqTelemetryUpdate => Unit = _ => (),
      domainRebuildStartCallback: Unit => Unit = _ => (),
      domainRebuildFinishCallback: Unit => Unit = _ => (),
      seedsSpec: F[Set[ModelParameterCollection[F]]]
  ): F[SlqIntegratedPosterior[F]] =
    for {
      sampleDomain             <- TxnVar.of(PointInCubeCollection.empty)
      samplePool               <- TxnVarMap.of(Map[Double, ModelParameterCollection[F]]())
      samplePoolMinimumLogPdf  <- TxnVar.of(-Double.MaxValue)
      logPdfResults            <- TxnVar.of(Vector[(Double, ModelParameterCollection[F])]())
      sampleDomainScalingState <- TxnVar.of(QuadratureDomainTelemetry.init)
      workTokenPool            <- TxnVar.of(0)
      abscissas                <- TxnVar.of(QuadratureAbscissaCollection.init)
      quadratureIntegrations   <- TxnVar.of(QuadratureIntegrator.empty)
      samplingSimulation       <- TxnVar.of(SamplingSimulation.empty)
      isConverged              <- TxnVar.of(false)
      seeds                    <- seedsSpec
    } yield SlqIntegratedPosterior(
      slqConfig = slqConfig,
      slqTelemetryUpdateCallback = slqTelemetryUpdateCallback,
      domainRebuildStartCallback = domainRebuildStartCallback,
      domainRebuildFinishCallback = domainRebuildFinishCallback,
      seeds = seeds,
      priors = posterior.priors,
      likelihoods = posterior.likelihoods,
      sampleDomain = sampleDomain,
      samplePool = samplePool,
      samplePoolMinimumLogPdf = samplePoolMinimumLogPdf,
      logPdfResults = logPdfResults,
      sampleDomainScalingState = sampleDomainScalingState,
      workTokenPool = workTokenPool,
      abscissas = abscissas,
      quadratureIntegrations = quadratureIntegrations,
      samplingSimulation = samplingSimulation,
      isConverged = isConverged
    )

}
