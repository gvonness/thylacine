package ai.entrolution
package thylacine

import bengal.stm.STM

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object implicits {
  implicit val stm: STM[IO] = STM.runtime[IO].unsafeRunSync()
}
