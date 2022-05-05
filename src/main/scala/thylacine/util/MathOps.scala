package ai.entrolution
package thylacine.util

import thylacine.model.core.Erratum.{ResultOrErrIo, UnexpectedErratum}

object MathOps {

  def trapezoidalQuadrature(
      abscissa: List[Double],
      values: List[Double]
  ): ResultOrErrIo[Double] =
    if (abscissa.size == values.size && abscissa.size > 1) {
      trapezoidalQuadrature(abscissa.zip(values))
    } else {
      ResultOrErrIo.fromErratum(
        UnexpectedErratum("Malformed abscissa for trapezoidal quadrature")
      )
    }

  def trapezoidalQuadrature(
      graphPoints: List[(Double, Double)]
  ): ResultOrErrIo[Double] =
    if (
      graphPoints.size > 1 && graphPoints
        .map(_._1)
        .toSet
        .size == graphPoints.size
    ) {
      ResultOrErrIo.fromCalculation {
        val sorted = graphPoints.sortBy(_._1)
        sorted
          .dropRight(1)
          .zip(sorted.tail)
          .map { graphPointPair =>
            0.5 * (graphPointPair._1._2 + graphPointPair._2._2) * (graphPointPair._2._1 - graphPointPair._1._1)
          }
          .sum
      }
    } else {
      ResultOrErrIo.fromErratum(
        UnexpectedErratum("Malformed abscissa for trapezoidal quadrature")
      )
    }

  // Creates a discretised CDF that facilitates sampling over a
  // discrete set of outcomes via uniform sampling on [0, 1)
  def cdfStaircase(
      values: List[BigDecimal]
  ): ResultOrErrIo[List[(BigDecimal, BigDecimal)]] =
    for {
      cdfReversed <- ResultOrErrIo.fromCalculation {
                       values.foldLeft(List[BigDecimal](BigDecimal(0))) {
                         (i, j) =>
                           (i.head + j) :: i
                       }
                     }
      normalisedCdf <- ResultOrErrIo.fromCalculation {
                         cdfReversed.map(_ / cdfReversed.head).reverse
                       }
      result <- ResultOrErrIo.fromCalculation {
                  normalisedCdf.dropRight(1).zip(normalisedCdf.tail)
                }
    } yield result

}
