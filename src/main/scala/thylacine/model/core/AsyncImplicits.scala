package ai.entrolution
package thylacine.model.core

import cats.effect.kernel.Async

abstract class AsyncImplicits[F[_]](protected implicit val asyncF: Async[F])
