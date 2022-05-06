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
package thylacine.model.sampling

import bengal.stm._
import thylacine.model.components.likelihood._
import thylacine.model.components.posterior._
import thylacine.model.components.prior._
import thylacine.model.core.Erratum._
import thylacine.model.core.IndexedVectorCollection._
import thylacine.model.core._

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.implicits._

import scala.collection.immutable.Queue

/** Implementation of the Hamiltonian MCMC sampling algorithm
  */
trait HmcmcEngine[T <: Prior[_], U <: Likelihood[_, _]]
    extends ModelParameterSampleGenerator {
  this: Posterior[T, U] =>

  private val stm = STM.runtime[IO].unsafeRunSync()
  import HmcmcEngine._
  import stm._

  /*
   * - - -- --- ----- -------- -------------
   * Configuration
   * - - -- --- ----- -------- -------------
   */

  protected def simulationsBetweenSamples: Int
  protected def stepsInSimulation: Int
  protected def simulationEpsilon: Double
  protected def warmUpSimulationCount: Int

  protected def sampleRequestUpdateCallback: Int => Unit
  protected def sampleRequestSetCallback: Int => Unit

  /*
   * - - -- --- ----- -------- -------------
   * State variables
   * - - -- --- ----- -------- -------------
   */

  private val sampleRequests: TxnVar[Queue[SampleRequest]] =
    TxnVar.of(Queue[SampleRequest]()).unsafeRunSync()

  private val currentMcmcPosition: TxnVar[ModelParameterCollection] =
    TxnVar.of(IndexedVectorCollection.empty).unsafeRunSync()

  private val burnInComplete: TxnVar[Boolean] =
    TxnVar.of(false).unsafeRunSync()

  /*
   * - - -- --- ----- -------- -------------
   * HMCMC
   * - - -- --- ----- -------- -------------
   */

  def runLeapfrogAt(
      input: ModelParameterCollection,
      p: VectorContainer,
      gradLogPdf: ModelParameterCollection,
      iterationCount: Int = 1
  ): ResultOrErrIo[(ModelParameterCollection, VectorContainer)] =
    if (iterationCount > stepsInSimulation) {
      ResultOrErrIo.fromValue((input, p))
    } else {
      (for {
        p <- rawVectorToModelParameterCollection(p.rawVector)
        pNew =
          p.rawSumWith(gradLogPdf.rawScalarMultiplyWith(simulationEpsilon / 2))
        xNew = input.rawSumWith(pNew.rawScalarMultiplyWith(simulationEpsilon))
        gNew <- logPdfGradientAt(xNew)
        pNewNew <-
          modelParameterCollectionToRawVector(
            pNew.rawSumWith(
              gNew.rawScalarMultiplyWith(simulationEpsilon / 2)
            )
          )
      } yield (xNew, VectorContainer(pNewNew.toArray.toList), gNew)).flatMap {
        i =>
          runLeapfrogAt(i._1, i._2, i._3, iterationCount + 1)
      }
    }

  def getHamiltonianValue(p: VectorContainer, E: Double): Double =
    p.rawDotProductWith(p) / 2.0 - E

  def runDynamicSimulationFrom(
      input: ModelParameterCollection,
      maxIterations: Int,
      logPdfOpt: Option[Double] = None,
      gradLogPdfOpt: Option[ModelParameterCollection] = None,
      iterationCount: Int = 1
  ): ResultOrErrIo[ModelParameterCollection] =
    if (iterationCount <= maxIterations) {
      (for {
        logPdf <- logPdfOpt match {
                    case Some(res) => ResultOrErrIo.fromValue(res)
                    case _         => logPdfAt(input)
                  }
        gradLogPdf <- gradLogPdfOpt match {
                        case Some(res) => ResultOrErrIo.fromValue(res)
                        case _         => logPdfGradientAt(input)
                      }
        p           = VectorContainer.random(domainDimension)
        hamiltonian = getHamiltonianValue(p, logPdf)
        xAndPNew <- runLeapfrogAt(input, p, gradLogPdf)
        (xNew, pNew) = xAndPNew
        eNew <- logPdfAt(xNew)
        hNew = getHamiltonianValue(pNew, eNew)
        dH   = hNew - hamiltonian
        result <- if (dH < 0 || Math.random() < Math.exp(-dH)) {
                    for {
                      newGradLogPdf <- logPdfGradientAt(xNew)
                    } yield (xNew, eNew, newGradLogPdf)
                  } else {
                    ResultOrErrIo.fromValue((input, logPdf, gradLogPdf))
                  }
      } yield result).flatMap { r =>
        runDynamicSimulationFrom(r._1,
                                 maxIterations,
                                 Some(r._2),
                                 Some(r._3),
                                 iterationCount + 1
        )
      }
    } else {
      ResultOrErrIo.fromValue(input)
    }

  /*
   * - - -- --- ----- -------- -------------
   * Sampling calculation
   * - - -- --- ----- -------- -------------
   */

  private def setNewSample(): Txn[Unit] =
    for {
      position <- currentMcmcPosition.get
      newPosition = runDynamicSimulationFrom(position,
                                             simulationsBetweenSamples
                    ).value.unsafeRunSync() // stateless computation
      _ <- newPosition match {
             case Right(newPosition) if newPosition != position =>
               currentMcmcPosition.set(newPosition)
             case Right(_) =>
               setNewSample()
             case Left(err) =>
               stm.abort(new RuntimeException(err.message))
           }
    } yield ()

  private def getNextRequest: Txn[SampleRequest] =
    for {
      requests <- sampleRequests.get
      _        <- stm.waitFor(requests.nonEmpty)
      result   <- stm.pure(requests.dequeue)
      _        <- sampleRequests.set(result._2)
    } yield result._1

  private def processRequest: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      for {
        result <- (for {
                    request   <- getNextRequest
                    _         <- setNewSample()
                    newSample <- currentMcmcPosition.get
                    remaining <- sampleRequests.get.map(_.size)
                  } yield (request, newSample, remaining)).commit
        _ <- IO(sampleRequestUpdateCallback(result._3)).start
        _ <- result._1.complete(result._2)
      } yield ()
    }

  private def processingRecursion: ResultOrErrIo[Unit] =
    processRequest.flatMap(_ => processingRecursion)

  /*
   * - - -- --- ----- -------- -------------
   * Initialisation
   * - - -- --- ----- -------- -------------
   */

  private def samplePriors: ResultOrErrIo[ModelParameterCollection] =
    for {
      sampleCollection <- priors.toList.parTraverse(_.sampleModelParameters)
    } yield sampleCollection.reduce(_ rawMergeWith _)

  private def burnIn: Txn[Unit] =
    for {
      burnInCompleted <- burnInComplete.get
      _ <- if (burnInCompleted) {
             stm.unit
           } else {
             val startPosition: ResultOrErrIo[ModelParameterCollection] = for {
               priorSample <- samplePriors
               result <-
                 runDynamicSimulationFrom(priorSample, warmUpSimulationCount)
             } yield result

             startPosition.value.unsafeRunSync() match {
               case Right(result) =>
                 for {
                   _ <- currentMcmcPosition.set(result)
                   _ <- burnInComplete.set(true)
                 } yield ()
               case Left(err) =>
                 stm.abort(new RuntimeException(err.message))
             }
           }
    } yield ()

  /*
   * - - -- --- ----- -------- -------------
   * Public Interfaces
   * - - -- --- ----- -------- -------------
   */

  def initialise: ResultOrErrIo[Unit] =
    for {
      _ <- ResultOrErrIo.fromCalculation(println("Burn in starting!"))
      _ <- ResultOrErrIo.fromIo(burnIn.commit)
      _ <- ResultOrErrIo.fromCalculation(println("Burn in completed!"))
      _ <- ResultOrErrIo.fromIo(processingRecursion.value.start)
    } yield ()

  def initialise(input: ModelParameterCollection): ResultOrErrIo[Unit] =
    for {
      _ <- ResultOrErrIo.fromIo {
             (for {
               _ <- currentMcmcPosition.set(input)
               _ <- burnInComplete.set(true)
             } yield ()).commit
           }
      _ <- ResultOrErrIo.fromIo(processingRecursion.value.start)
    } yield ()

  def waitForInitialisation: ResultOrErrIo[Unit] =
    ResultOrErrIo.fromIo {
      (for {
        burnInCompleted <- burnInComplete.get
        _               <- stm.waitFor(burnInCompleted)
      } yield ()).commit
    }

  protected def getHmcmcSample: ResultOrErrIo[ModelParameterCollection] =
    ResultOrErrIo.fromIo {
      for {
        deferred <- Deferred[IO, ModelParameterCollection]
        newRequestSize <- (for {
               _                 <- sampleRequests.modify(_.enqueue(deferred))
               sampleRequestSize <- sampleRequests.get.map(_.size)
             } yield sampleRequestSize).commit
        _ <- IO(sampleRequestSetCallback(newRequestSize)).start
        result <- deferred.get
      } yield result
    }

}

object HmcmcEngine {

  type SampleRequest = Deferred[IO, ModelParameterCollection]
}
