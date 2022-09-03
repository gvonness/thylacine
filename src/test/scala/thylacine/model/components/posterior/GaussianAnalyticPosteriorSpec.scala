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
package thylacine.model.components.posterior

import bengal.stm.STM
import ai.entrolution.thylacine.model.components.ComponentFixture._
import thylacine.TestUtils._

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class GaussianAnalyticPosteriorSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "GaussianAnalyticPosterior" - {

    // Unfortunately, the analytic solution has a number of matrix inversions
    // that chip away at the numerical accuracy of the result
    "generate the correct mean for the inference" in {
      (for {
        implicit0(stm: STM[IO]) <- STM.runtime[IO]
        posterior               <- analyticPosteriorF
      } yield maxIndexVectorDiff(posterior.mean, Map("foo" -> Vector(1, 2), "bar" -> Vector(5))))
        .asserting(_ shouldBe (0d +- .1))
    }

    // Not a great test, but the prior and likelihood align
    // on the inference for the parameters (i.e. all uncertainties should
    // be very small)
    "generate the correct covariance" in {
      (for {
        implicit0(stm: STM[IO]) <- STM.runtime[IO]
        posterior               <- analyticPosteriorF
      } yield maxVectorDiff(posterior.covarianceStridedVector, Vector.fill(9)(0d))).asserting(_ shouldBe (0.0 +- .01))
    }
  }
}
