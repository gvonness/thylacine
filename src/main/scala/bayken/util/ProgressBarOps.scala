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
package bayken.util

import bengal.stm.STM

import cats.effect.IO
import cats.effect.unsafe.implicits.global

// Singleton to control progress bar command line visualisation
object ProgressBarOps {
  val stm: STM[IO] = STM.runtime[IO].unsafeRunSync()
  import stm._

  private val progressBar =
    TxnVar.of(new ProgressBar(0)).unsafeRunSync()

  def set(amount: Int): Unit =
    progressBar.set(new ProgressBar(amount)).commit.unsafeRunSync()

  def increment(): Unit =
    progressBar.modify { i =>
      i.add(1)
      i
    }.commit.unsafeRunSync()

  def finish(): Unit =
    progressBar.modify { i =>
      i.finish()
      i
    }.commit.unsafeRunSync()
}
