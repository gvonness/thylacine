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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import TestUtils._
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper

class AnalyticPosteriorSpec extends AnyFlatSpec {
  // Unfortunately, the analytic solution has a number of matrix inversions
  // that chip away at the numerical accuracy of the result
  "GaussianAnalyticPosterior" should "generate the correct mean for the inference" in new InferenceFixture {
    maxIndexVectorDiff(analyticPosterior.mean, Map("foo" -> Vector(1, 2), "bar" -> Vector(5))) shouldBe (.05 +- .05)
  }

  // Not a great test, but the prior and likelihood align
  // on the inference for the parameters (i.e. all uncertainties should
  // be very small)
  it should "generate the correct covariance" in new InferenceFixture {
    maxVectorDiff(analyticPosterior.covarianceStridedVector, Vector.fill(9)(0d)) shouldBe (0.0 +- .01)
  }
}
