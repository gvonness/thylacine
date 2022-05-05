package ai.entrolution
package thylacine.model.components.prior

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

trait Prior[+BM <: BeliefModel]
    extends ModelParameterPdf
    with PosteriorTerm
    with ModelParameterSampleGenerator
    with ModelParameterGenerator
    with CanValidate[Prior[_]] {

  def priorModel: BM

  override final val posteriorTermIdentifier: TermIdentifier = TermIdentifier(
    identifier.value
  )

  override final val domainDimension: Int    = priorModel.domainDimension
  override final val generatorDimension: Int = priorModel.domainDimension

  override final def logPdfAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[Double] =
    for {
      vector <- input.retrieveIndex(identifier)
      res    <- priorModel.logPdfAt(vector)
    } yield res

  override final def logPdfGradientAt(
      input: IndexedVectorCollection
  ): ResultOrErrIo[ModelParameterCollection] =
    for {
      vector  <- input.retrieveIndex(identifier)
      gradLog <- priorModel.logPdfGradientAt(vector)
      res <- ResultOrErrIo.fromCalculation(
               IndexedVectorCollection(identifier, gradLog)
             )
    } yield res

  override final def sampleModelParameters
      : ResultOrErrIo[ModelParameterCollection] =
    for {
      vectorResult <- rawSampleModelParameters
    } yield IndexedVectorCollection(identifier, vectorResult)
}
