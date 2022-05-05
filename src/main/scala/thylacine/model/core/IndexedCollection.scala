package ai.entrolution
package thylacine.model.core

import thylacine.model.core.Erratum._
import thylacine.model.core.GenericIdentifier._

trait IndexedCollection[T <: Container] {
  def index: Map[ModelParameterIdentifier, T]

  def retrieveIndex(identifier: ModelParameterIdentifier): ResultOrErrIo[T] =
    ResultOrErrIo.fromResultOrErr {
      index
        .get(identifier)
        .map(Right(_))
        .getOrElse(
          Left(
            UnexpectedErratum(
              s"Identifier $identifier not found in indexed collection: $index"
            )
          )
        )
    }

  def getSortedValues: ResultOrErrIo[List[T]] =
    ResultOrErrIo.fromCalculation(index.toList.sortBy(_._1).map(_._2))
}
