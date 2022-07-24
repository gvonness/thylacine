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
