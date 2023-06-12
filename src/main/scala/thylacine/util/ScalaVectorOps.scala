/*
 * Copyright 2020-2023 Greg von Nessi
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
package thylacine.util

object ScalaVectorOps {

  object Implicits {

    implicit class VectorOps(input: Vector[Double]) {

      lazy val magnitudeSquared: Double =
        MathOps.vectorMagnitudeSquared(input)

      lazy val magnitude: Double =
        Math.sqrt(magnitudeSquared)

      def dotProductWith(input2: Vector[Double]): Double =
        MathOps.vectorDotProduct(input, input2)

      def scalarMultiplyWith(multiplier: Double): Vector[Double] =
        MathOps.scalarMultipleVector(input, multiplier)

      def add(input2: Vector[Double]): Vector[Double] =
        MathOps.vectorAddition(input, input2)

      def subtract(input2: Vector[Double]): Vector[Double] =
        MathOps.vectorAddition(input, MathOps.scalarMultipleVector(input2, -1))
    }

  }

}
