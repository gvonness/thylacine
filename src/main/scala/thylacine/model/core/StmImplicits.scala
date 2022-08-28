package ai.entrolution
package thylacine.model.core

import bengal.stm.STM

import cats.effect.kernel.Async

abstract class StmImplicits[F[_]](protected implicit val stmF: STM[F], override protected implicit val asyncF: Async[F])
    extends AsyncImplicits[F]
