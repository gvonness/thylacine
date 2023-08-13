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
package thylacine.visualisation

private[thylacine] case class SimplexChain(chain: Seq[Simplex]) {
  private[thylacine] lazy val getPoints: Vector[GraphPoint] = chain.map(_.start).toVector

  private[thylacine] def linearInterpolationUsing(ds: Double): SimplexChain =
    SimplexChain(
      chain
        .foldLeft((0d, Vector[GraphPoint]())) { (i, j) =>
          val nextInterpolation = j.getInterpolationPoints(i._1, ds)
          (nextInterpolation._2, i._2 ++ nextInterpolation._1)
        }
        ._2
    )
}

private[thylacine] object SimplexChain {

  private[thylacine] def apply(input: Vector[GraphPoint]): SimplexChain =
    if (input.size >= 2) {
      SimplexChain(input.dropRight(1).zip(input.drop(1)).map(Simplex(_)))
    } else {
      SimplexChain(Vector[Simplex]())
    }
}
