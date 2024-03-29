/*
 * Copyright 2023 Greg von Nessi
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
package thylacine.model.components.posterior

import bengal.stm.STM
import bengal.stm.model.{ TxnVar, TxnVarMap }
import thylacine.config.SlqConfig
import thylacine.model.components.likelihood.Likelihood
import thylacine.model.components.prior.Prior
import thylacine.model.core.StmImplicits
import thylacine.model.core.telemetry.SlqTelemetryUpdate
import thylacine.model.core.values.IndexedVectorCollection
import thylacine.model.core.values.IndexedVectorCollection.ModelParameterCollection
import thylacine.model.integration.slq.*

import cats.effect.kernel.Async
import cats.syntax.all.*

import scala.annotation.unused

case class SlqIntegratedPosterior[F[_]: STM: Async](
  private[thylacine] val slqConfig: SlqConfig,
  override protected val slqTelemetryUpdateCallback: SlqTelemetryUpdate => F[Unit],
  override protected val domainRebuildStartCallback: Unit => F[Unit],
  override protected val domainRebuildFinishCallback: Unit => F[Unit],
  override protected val seeds: Set[ModelParameterCollection],
  override private[thylacine] val priors: Set[Prior[F, ?]],
  override private[thylacine] val likelihoods: Set[Likelihood[F, ?, ?]],
  override protected val sampleDomain: TxnVar[F, PointInCubeCollection],
  override protected val samplePool: TxnVarMap[F, Double, ModelParameterCollection],
  override protected val samplePoolMinimumLogPdf: TxnVar[F, Double],
  override protected val logPdfResults: TxnVar[F, Vector[(Double, ModelParameterCollection)]],
  override protected val sampleDomainScalingState: TxnVar[F, QuadratureDomainTelemetry],
  override protected val workTokenPool: TxnVar[F, Int],
  override protected val abscissas: TxnVar[F, QuadratureAbscissaCollection],
  override protected val quadratureIntegrations: TxnVar[F, QuadratureIntegrator],
  override protected val samplingSimulation: TxnVar[F, SamplingSimulation],
  override protected val isConverged: TxnVar[F, Boolean]
) extends StmImplicits[F]
    with Posterior[F, Prior[F, ?], Likelihood[F, ?, ?]]
    with SlqEngine[F] {
  final override protected val slqSamplePoolSize: Int = slqConfig.poolSize

  final override protected val slqNumberOfAbscissa: Int =
    slqConfig.abscissaNumber

  final override protected val slqScalingIncrement: Double =
    slqConfig.domainScalingIncrement

  final override protected val slqNominalAcceptanceRatio: Double =
    slqConfig.targetAcceptanceProbability

  final override protected val slqSampleParallelism: Int =
    slqConfig.sampleParallelism

  final override protected val maxIterationCount: Int = slqConfig.maxIterationCount

  final override protected val minIterationCount: Int = slqConfig.minIterationCount

  @unused
  def rebuildSampleSimulation: F[Unit] =
    for {
      _ <- buildSampleSimulation
      _ <- waitForSimulationConstruction
    } yield ()
}

@unused
object SlqIntegratedPosterior {

  @unused
  def of[F[_]: STM: Async](
    slqConfig: SlqConfig,
    posterior: Posterior[F, Prior[F, ?], Likelihood[F, ?, ?]],
    slqTelemetryUpdateCallback: SlqTelemetryUpdate => F[Unit],
    domainRebuildStartCallback: Unit => F[Unit],
    domainRebuildFinishCallback: Unit => F[Unit],
    seedsSpec: F[Set[Map[String, Vector[Double]]]]
  ): F[SlqIntegratedPosterior[F]] =
    for {
      sampleDomain             <- TxnVar.of(PointInCubeCollection.empty)
      samplePool               <- TxnVarMap.of(Map[Double, ModelParameterCollection]())
      samplePoolMinimumLogPdf  <- TxnVar.of(-Double.MaxValue)
      logPdfResults            <- TxnVar.of(Vector[(Double, ModelParameterCollection)]())
      sampleDomainScalingState <- TxnVar.of(QuadratureDomainTelemetry.init)
      workTokenPool            <- TxnVar.of(0)
      abscissas                <- TxnVar.of(QuadratureAbscissaCollection.init)
      quadratureIntegrations   <- TxnVar.of(QuadratureIntegrator.empty)
      samplingSimulation       <- TxnVar.of(SamplingSimulation.empty)
      isConverged              <- TxnVar.of(false)
      seeds                    <- seedsSpec.map(seeds => seeds.map(IndexedVectorCollection(_)))
    } yield SlqIntegratedPosterior(
      slqConfig                   = slqConfig,
      slqTelemetryUpdateCallback  = slqTelemetryUpdateCallback,
      domainRebuildStartCallback  = domainRebuildStartCallback,
      domainRebuildFinishCallback = domainRebuildFinishCallback,
      seeds                       = seeds,
      priors                      = posterior.priors,
      likelihoods                 = posterior.likelihoods,
      sampleDomain                = sampleDomain,
      samplePool                  = samplePool,
      samplePoolMinimumLogPdf     = samplePoolMinimumLogPdf,
      logPdfResults               = logPdfResults,
      sampleDomainScalingState    = sampleDomainScalingState,
      workTokenPool               = workTokenPool,
      abscissas                   = abscissas,
      quadratureIntegrations      = quadratureIntegrations,
      samplingSimulation          = samplingSimulation,
      isConverged                 = isConverged
    )

}
