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
package thylacine.model.components.likelihood

import bengal.stm.STM
import thylacine.TestUtils.maxIndexVectorDiff
import thylacine.model.components.ComponentFixture.fooNonAnalyticLikeliHoodF
import thylacine.model.core.values.IndexedVectorCollection

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class GaussianLikelihoodSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "GaussianLikelihood" - {

    "generate the a zero gradient at the likelihood maximum" in {
      (for {
        implicit0(stm: STM[IO]) <- STM.runtime[IO]
        likelihood              <- fooNonAnalyticLikeliHoodF
        result                  <- likelihood.logPdfGradientAt(IndexedVectorCollection(Map("foo" -> Vector(1d, 2d))))
      } yield result.genericScalaRepresentation)
        .asserting(_ shouldBe Map("foo" -> Vector(0d, 0d)))
    }

    "generate the correct gradient of the logPdf for a given point" in {
      (for {
        implicit0(stm: STM[IO]) <- STM.runtime[IO]
        likelihood              <- fooNonAnalyticLikeliHoodF
        result                  <- likelihood.logPdfGradientAt(IndexedVectorCollection(Map("foo" -> Vector(3d, 2d))))
      } yield result.genericScalaRepresentation)
        .asserting(result => maxIndexVectorDiff(result, Map("foo" -> Vector(-4e5, -88e4))) shouldBe (0d +- 1e-4))
    }
  }
}
