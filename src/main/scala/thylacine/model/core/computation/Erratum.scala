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
package thylacine.model.core.computation

private[thylacine] sealed trait Erratum {
  private[thylacine] def message: String

  override def toString: String =
    message

  private[thylacine] def toSingleLineString: String =
    message.filter(_ >= ' ')
}

private[thylacine] object Erratum {
  private[thylacine] case class TransientErratum(message: String) extends Erratum

  private[thylacine] object TransientErratum {

    private[thylacine] def apply(ex: Throwable): TransientErratum =
      TransientErratum(s"""Unexpected transient exception encountered
                          |Exception message: ${ex.getMessage}
                          |Exception cause: ${ex.getCause}
                          |Exception stacktrace: ${ex.getStackTrace
        .mkString("Array(", ", ", ")")}""".stripMargin)
  }

  private[thylacine] sealed trait PersistentErratum extends Erratum

  private[thylacine] case class UnexpectedErratum(message: String) extends PersistentErratum

  private[thylacine] object UnexpectedErratum {

    private[thylacine] def apply(ex: Throwable): UnexpectedErratum =
      UnexpectedErratum(s"""Unexpected persistent exception encountered
                           |Exception message: ${ex.getMessage}
                           |Exception cause: ${ex.getCause}
                           |Exception stacktrace: ${ex.getStackTrace
        .mkString("Array(", ", ", ")")}""".stripMargin)
  }

  private[thylacine] case class MinorErratum(message: String) extends PersistentErratum
}
