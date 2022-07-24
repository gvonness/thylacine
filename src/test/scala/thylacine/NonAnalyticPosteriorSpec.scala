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
package thylacine

import thylacine.TestUtils._

import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import cats.effect.unsafe.implicits.global

class NonAnalyticPosteriorSpec extends AnyFlatSpec {
  // Unfortunately, the analytic solution has a number of matrix inversions
  // that chip away at the numerical accuracy of the result
  "HookesAndJeevesOptimisedPosterior" should "find the parameters that correspond to the posterior maximum" in new InferenceFixture {
    maxIndexVectorDiff(posteriorOptimiser.findMaximumLogPdf.unsafeRunSync()._2,
                       Map("fooniform" -> Vector(1, 2), "barniform" -> Vector(5))
    ) shouldBe (0.0 +- 1e-5)
  }
}
