/*
 * Copyright 2020-2023 Greg von Nessi
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
package thylacine.model.components.forwardmodel

import bengal.stm.STM
import thylacine.model.core.StmImplicits
import thylacine.model.core.computation.{CachedComputation, FiniteDifferenceJacobian}
import thylacine.model.core.values.{IndexedMatrixCollection, IndexedVectorCollection, VectorContainer}

import cats.effect.kernel.Async
import cats.syntax.all._

case class NonLinearForwardModel[F[_]: STM: Async](
    protected override val evalCache: CachedComputation[F, VectorContainer],
    protected override val jacobianCache: CachedComputation[F, IndexedMatrixCollection],
    private val domainDimensions: Map[String, Int],
    override val rangeDimension: Int,
    private[thylacine] override val validated: Boolean = false
) extends StmImplicits[F]
    with InMemoryMemoizedForwardModel[F] {

  private[thylacine] override val getValidated = this

  override val domainDimension: Int = domainDimensions.values.sum
}

object NonLinearForwardModel {

  def of[F[_]: STM: Async](
      evaluation: Map[String, Vector[Double]] => Vector[Double],
      differential: Double,
      domainDimensions: Map[String, Int],
      rangeDimension: Int,
      evalCacheDepth: Option[Int],
      jacobianCacheDepth: Option[Int]
  ): F[NonLinearForwardModel[F]] = {
    def transformedEval(input: IndexedVectorCollection): VectorContainer =
      VectorContainer(evaluation(input.index.map(i => i._1.value -> i._2.scalaVector)))

    val jacobianCalculation = FiniteDifferenceJacobian(transformedEval, differential)

    for {
      evalCache <- CachedComputation.of(transformedEval, evalCacheDepth)
      jacobianCache <-
        CachedComputation.of(jacobianCalculation.finiteDifferenceJacobianAt, jacobianCacheDepth)
    } yield NonLinearForwardModel[F](evalCache, jacobianCache, domainDimensions, rangeDimension)
  }

  def of[F[_]: STM: Async](
      evaluation: Map[String, Vector[Double]] => Vector[Double],
      jacobian: Map[String, Vector[Double]] => Map[String, Vector[
        Vector[Double]
      ]],
      domainDimensions: Map[String, Int],
      rangeDimension: Int,
      evalCacheDepth: Option[Int],
      jacobianCacheDepth: Option[Int]
  ): F[NonLinearForwardModel[F]] = {

    def transformedEval(input: IndexedVectorCollection): VectorContainer =
      VectorContainer(evaluation(input.index.map(i => i._1.value -> i._2.scalaVector)))

    def transformedJacobian(input: IndexedVectorCollection): IndexedMatrixCollection =
      IndexedMatrixCollection(jacobian(input.index.map(i => i._1.value -> i._2.scalaVector)))

    for {
      evalCache     <- CachedComputation.of(transformedEval, evalCacheDepth)
      jacobianCache <- CachedComputation.of(transformedJacobian, jacobianCacheDepth)
    } yield NonLinearForwardModel[F](evalCache, jacobianCache, domainDimensions, rangeDimension)
  }
}
