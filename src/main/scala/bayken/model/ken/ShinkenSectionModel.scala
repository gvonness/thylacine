package ai.entrolution
package bayken.model.ken

import bayken.numerical.Polynomial1D.NonTrivialPolynomial
import bayken.numerical.{LegendreQuadrature, Point2D, Polynomial1D}

case class ShinkenSectionModel(
    shinkenSection: ShinkenSectionLabel,
    points: Set[Point2D],
    lowerBound: Double,
    upperBound: Double,
    massInferenceQuadratureSize: Int
) {
  assert(points.nonEmpty)

  lazy val quadrature: LegendreQuadrature = LegendreQuadrature(massInferenceQuadratureSize)

  lazy val polesAndWeights: Seq[(Double, Double)] =
    quadrature.getPolesAndWeights(lowerBound, upperBound)

  lazy val fitPolynomial: NonTrivialPolynomial =
    Polynomial1D.fit(points.toList)

  def getFitPolynomial(derivatives: List[Point2D]): NonTrivialPolynomial =
    Polynomial1D.fit(points.toList, derivatives)
}

object ShinkenSectionModel {

  def apply(
      shinkenSection: ShinkenSectionLabel,
      points: Set[Point2D],
      quadratureSize: Int
  ): ShinkenSectionModel =
    ShinkenSectionModel(
      shinkenSection = shinkenSection,
      points = points,
      lowerBound = points.map(_.x).min,
      upperBound = points.map(_.x).max,
      massInferenceQuadratureSize = quadratureSize
    )
}
