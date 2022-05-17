package ai.entrolution
package bayken.model

import bayken.numerical.LegendreQuadratureBuilder

sealed trait Abscissa {
  def min: Double
  def max: Double
  def abscissaPoints: List[Double]
  def scale: Double = max - min
}

object Abscissa {

  case class LegendreAbscissa(
      quadrature: LegendreQuadratureBuilder
  ) extends Abscissa {

    lazy val abscissaPoints: List[Double] =
      quadrature.poles

    override lazy val min: Double = abscissaPoints.min

    override lazy val max: Double = abscissaPoints.max
  }

  case class UniformAbscissa(min: Double, max: Double, numPoints: Int)
      extends Abscissa {
    private val intervalLength: Double = max - min
    lazy val differential: Double      = (max - min) / (numPoints - 1)

    lazy val abscissaPoints: List[Double] = (0 until numPoints)
      .map(i => min + i.toDouble / (numPoints - 1) * intervalLength)
      .toList
  }
}
