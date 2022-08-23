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
package thylacine.model.core.values.modelparameters

import thylacine.model.core.GenericScalarValuedMapping

import cats.effect.kernel.Async

private[thylacine] abstract class ModelParameterPdf[F[_]: Async] extends GenericScalarValuedMapping {

  private[thylacine] def logPdfAt(
      input: ModelParameterCollection
  ): ResultOrErrF[F, Double]

  // Will work most of the time but will require
  // adjustment for pathological cases (e.g. Uniform distributions)
  private[thylacine] def pdfAt(
      input: ModelParameterCollection
  ): ResultOrErrF[F, Double] =
    logPdfAt(input).map(Math.exp)

  private[thylacine] def logPdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrF[F, ModelParameterCollection]

  // Will work most of the time but will require
  // adjustment for pathological cases (e.g. Uniform distributions)
  private[thylacine] def pdfGradientAt(
      input: ModelParameterCollection
  ): ResultOrErrF[F, ModelParameterCollection] =
    for {
      pdf      <- pdfAt(input)
      gradLogs <- logPdfGradientAt(input)
    } yield gradLogs.index.toList.map { gl =>
      IndexedVectorCollection(gl._1, VectorContainer(pdf * gl._2.rawVector))
    }.reduce(_ rawMergeWith _)

  final def logPdfAt(input: Map[String, Vector[Double]]): F[Double] =
    logPdfAt(IndexedVectorCollection(input)).liftToF

  final def pdfAt(input: Map[String, Vector[Double]]): F[Double] =
    logPdfAt(input).map(Math.exp)

  final def logPdfGradientAt(
      input: Map[String, Vector[Double]]
  ): F[Map[String, Vector[Double]]] =
    logPdfGradientAt(IndexedVectorCollection(input))
      .map(_.genericScalaRepresentation)
      .liftToF

  final def pdfGradientAt(
      input: Map[String, Vector[Double]]
  ): F[Map[String, Vector[Double]]] =
    pdfGradientAt(IndexedVectorCollection(input))
      .map(_.genericScalaRepresentation)
      .liftToF
}
