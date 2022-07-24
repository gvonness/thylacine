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
