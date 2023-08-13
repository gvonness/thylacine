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
package thylacine.model.components.posterior

import bengal.stm.STM
import thylacine.TestUtils.*
import thylacine.model.components.ComponentFixture.mdsOptimisedPosteriorF

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class MdsOptimisedPosteriorSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "MdsOptimisedPosterior" - {
    "find the parameters that correspond to the posterior maximum" in {
      (for {
        case implicit0(stm: STM[IO]) <- STM.runtime[IO]
        posterior               <- mdsOptimisedPosteriorF
        optimisationResult      <- posterior.findMaximumLogPdf(Map())
      } yield maxIndexVectorDiff(optimisationResult._2, Map("fooniform" -> Vector(1, 2), "barniform" -> Vector(5))))
        .asserting(_ shouldBe (0.0 +- 1e5))
    }
  }
}
