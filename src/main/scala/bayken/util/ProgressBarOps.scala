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
