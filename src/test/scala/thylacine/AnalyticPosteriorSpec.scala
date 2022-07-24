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
