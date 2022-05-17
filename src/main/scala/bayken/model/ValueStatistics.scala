package ai.entrolution
package bayken.model

import bayken.numerical.LegendreQuadratureBuilder

case class ValueStatistics(
    quadrature: LegendreQuadratureBuilder,
    values: List[Double]
) {
  assert(quadrature.order == values.size)

  lazy val definiteIntegral: Double =
    quadrature.weights.zip(values).map(i => i._1 * i._2).sum

  lazy val normalisedCdf: List[Double] = {
    val (total, unscaledAccumulation) =
      values
        .zip(quadrature.weights)
        .map(i => i._1 * i._2)
        .foldLeft((0d, List[Double]())) { (i, j) =>
          val currentSum = i._1 + j
          (currentSum, currentSum :: i._2)
        }

    unscaledAccumulation.reverse.map(_ / total)
  }

  lazy val median: Double =
    normalisedCdf
      .zip(quadrature.poles)
      .minBy(i => Math.abs(i._1 - .5))
      ._2

  lazy val integratedMean: Double = {
    quadrature.weights
      .zip(quadrature.poles)
      .map(i => i._1 * i._2)
      .zip(values)
      .map(i => i._1 * i._2)
      .sum / definiteIntegral
  }

  lazy val standardDeviation: Double =
    Math.sqrt(
      quadrature.weights
        .zip(quadrature.poles)
        .map(i => i._1 * Math.pow(i._2 - integratedMean, 2))
        .zip(values)
        .map(i => i._1 * i._2)
        .sum / definiteIntegral
    )

  def centralNormalisedMoment(momentOrder: Int): Double =
    quadrature.weights
      .zip(quadrature.poles)
      .map(i => i._1 * Math.pow(i._2 - integratedMean, momentOrder.toDouble))
      .zip(values)
      .map(i => i._1 * i._2)
      .sum / Math.pow(integratedMean, momentOrder.toDouble)

  lazy val valueMean: Double =
    values.sum / values.size

  lazy val (valueConfidenceInterval95Min, valueConfidenceInterval95Max) = {
    val result = values
      .sortBy(i => Math.abs(i - valueMean))
      .dropRight(Math.ceil(values.size * .05).toInt)
    (result.min, result.max)
  }
}
