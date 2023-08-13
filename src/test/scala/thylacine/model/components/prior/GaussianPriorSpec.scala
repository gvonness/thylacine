/*
 * Copyright 2023 Greg von Nessi
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
package thylacine.model.components.prior

import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GaussianPriorSpec extends AnyFlatSpec with should.Matchers {
  "GaussianPrior" should "generate the correct mean and covariance for a 1D distribution" in {
    val fooPrior: GaussianPrior[IO] =
      GaussianPrior.fromConfidenceIntervals[IO](
        label               = "foo",
        values              = Vector(2),
        confidenceIntervals = Vector(3)
      )

    fooPrior.mean shouldBe Vector(2)
    fooPrior.covariance shouldBe Vector(Math.pow(3.0 / 2, 2))
  }

  it should "generate the correct mean and covariance for a multi-variate distribution" in {
    val fooPrior: GaussianPrior[IO] =
      GaussianPrior.fromConfidenceIntervals[IO](
        label               = "foo",
        values              = Vector(2, 3, 4),
        confidenceIntervals = Vector(4, 3, 2)
      )

    fooPrior.mean shouldBe Vector(2, 3, 4)
    fooPrior.covariance shouldBe Vector(4.0, 0, 0, 0, Math.pow(3.0 / 2, 2), 0, 0, 0, 1)
  }

  it should "generate the correct gradient of the logPdf" in {
    val fooPrior: GaussianPrior[IO] =
      GaussianPrior.fromConfidenceIntervals[IO](
        label               = "foo",
        values              = Vector(2, 3, 4),
        confidenceIntervals = Vector(4, 3, 2)
      )

    fooPrior.rawLogPdfGradientAt(Vector(3, 2, 5)) shouldBe Vector(-.25, 4.0 / 9, -1)
  }
}
