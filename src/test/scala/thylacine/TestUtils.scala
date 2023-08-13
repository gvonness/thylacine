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
package thylacine

object TestUtils {

  def maxVectorDiff(input1: Vector[Double], input2: Vector[Double]): Double = {
    assert(input1.size == input2.size)
    input1.zip(input2).map(i => Math.abs(i._1 - i._2)).max
  }

  def maxMatrixDiff(input1: Vector[Vector[Double]], input2: Vector[Vector[Double]]): Double = {
    assert(input1.size == input2.size)
    input1.zip(input2).map(i => maxVectorDiff(i._1, i._2)).max
  }

  def maxIndexVectorDiff(input1: Map[String, Vector[Double]], input2: Map[String, Vector[Double]]): Double = {
    assert(input1.keySet == input2.keySet)
    input1.map(i => maxVectorDiff(i._2, input2(i._1))).max
  }
}
