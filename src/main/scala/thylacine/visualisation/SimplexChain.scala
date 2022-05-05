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
package thylacine.visualisation

case class SimplexChain(chain: Seq[Simplex]) {
  lazy val getPoints: List[GraphPoint] = chain.map(_.start).toList

  def reinterp(ds: Double): SimplexChain =
    SimplexChain(
      chain
        .foldLeft((0d, List[GraphPoint]())) { (i, j) =>
          val nextInterp = j.getInterpolationPoints(i._1, ds)
          (nextInterp._2, i._2 ++ nextInterp._1)
        }
        ._2
    )
}

object SimplexChain {

  def apply(input: List[GraphPoint]): SimplexChain =
    if (input.size >= 2) {
      SimplexChain(input.dropRight(1).zip(input.drop(1)).map(Simplex(_)))
    } else {
      SimplexChain(List[Simplex]())
    }
}
