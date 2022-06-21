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
package bayken.model.ken

import bayken.config.measurements.MunePointConfig
import bayken.numerical.arclength.{ArcLengthFromZero, ArcLengthFromZeroInverse}
import bayken.numerical.{
  AnalyticRealValuedFunction,
  PiecewisePolynomial1DSupport,
  Point2D,
  RealValuedFunction
}

// Overrides are entry points for priors in non-analytic inferences
case class BladeGeometry(
    rawMunePoints: List[MunePointConfig],
    munePointXOverride: Option[List[Double]],
    munePointYOverride: Option[List[Double]],
    bladeWidthOverride: Option[List[Double]],
    arcLengthInversionAccuracy: Double = 0.00001
) {

  private val munePoints: List[MunePointConfig] = {
    val xProcessedPoints: List[MunePointConfig] = munePointXOverride match {
      case Some(xOverride) =>
        assert(xOverride.size == rawMunePoints.size)
        rawMunePoints.zip(xOverride).map(pts => pts._1.copy(x = pts._2))
      case _ =>
        rawMunePoints
    }

    val yProcessedPoints: List[MunePointConfig] = munePointYOverride match {
      case Some(yOverride) =>
        assert(yOverride.size == xProcessedPoints.size)
        xProcessedPoints.zip(yOverride).map(pts => pts._1.copy(y = pts._2))
      case _ =>
        xProcessedPoints
    }

    bladeWidthOverride match {
      case Some(depthOverride) =>
        val (pointsWithWidths, pointsWithoutWidths) =
          xProcessedPoints.partition(p => p.depth.isDefined)
        assert(pointsWithWidths.size == depthOverride.size)
        pointsWithWidths
          .zip(depthOverride)
          .map(pts => pts._1.copy(width = Some(pts._2))) ++ pointsWithoutWidths
      case _ =>
        yProcessedPoints
    }
  }

  private val kissakeBladeWidthFitOrder: Int = 2
  private val tangFitOrder: Int              = 2
  private val muneFitOrder: Int              = 6

  private def rotatePointsAboutOrigin(
      input: List[MunePointConfig],
      rotation: Double
  ): List[MunePointConfig] =
    input.map { i =>
      val newPoint = Point2D.rotateAboutOrigin(Point2D(i.x, i.y), rotation)
      i.copy(x = newPoint.x, y = newPoint.y)
    }

  // Translate points so the bottom of the tang, on the mune side,
  // is set to the origin, with the blade extending along the positive
  // x direction (curving below the x-axis). Additionally, we specify
  // zero derivative at the origin by rotating all the points so that
  // the second point has a y-value of 0. This is only the initial point
  // orientation.
  lazy val orientedPoints: List[MunePointConfig] = {
    val origin = munePoints.minBy(_.x)
    val shiftedPoints: Seq[MunePointConfig] = munePoints.map(p =>
      p.copy(x = Math.abs(p.x - origin.x), y = p.y - origin.y)
    )
    val tang: MunePointConfig = munePoints
      .find(_.tangBoundary)
      .getOrElse(munePoints.filter(_.tsubaBoundary).minBy(_.x))

    val rotation: Double = Math.atan2(tang.y, tang.x)

    rotatePointsAboutOrigin(shiftedPoints.toList, -rotation)
  }

  private val oTangPoint: Option[MunePointConfig] =
    orientedPoints.find(_.tangBoundary)

  private val tsubaPoints: Seq[MunePointConfig] =
    orientedPoints.filter(_.tsubaBoundary).sortBy(_.x)

  private val kissakePoint: MunePointConfig =
    orientedPoints.find(_.kissakeBoundary).get

  val baseMuneModel: PiecewisePolynomial1DSupport =
    oTangPoint match {
      case Some(tangPoint) =>
        PiecewisePolynomial1DSupport.fitPiecewisePolynomial(
          Seq(tangPoint.point2d.x),
          Seq(tangFitOrder, muneFitOrder),
          orientedPoints.map(_.point2d)
        )
      case None =>
        PiecewisePolynomial1DSupport.fitPiecewisePolynomial(
          tsubaPoints.map(_.x),
          Seq(tangFitOrder, 0, muneFitOrder),
          orientedPoints.map(_.point2d)
        )
    }

  private val baseArcLengthCalculation: ArcLengthFromZero =
    ArcLengthFromZero(baseMuneModel)

  private val bladeWidthVsArcLengthPoints: Seq[Point2D] =
    orientedPoints.flatMap(p =>
      p.depth.map(d => Point2D(baseArcLengthCalculation.evalAt(p.x), d))
    )

  val baseBladeWidthModel: PiecewisePolynomial1DSupport =
    oTangPoint match {
      case Some(tangPoint) =>
        PiecewisePolynomial1DSupport.fitPiecewisePolynomial(
          Seq(tangPoint.point2d.magnitude, kissakePoint.point2d.magnitude)
            .map(baseArcLengthCalculation.evalAt),
          Seq(tangFitOrder, muneFitOrder, kissakeBladeWidthFitOrder),
          bladeWidthVsArcLengthPoints
        )
      case None =>
        PiecewisePolynomial1DSupport.fitPiecewisePolynomial(
          (tsubaPoints.map(_.x) ++ Seq(kissakePoint.point2d.magnitude))
            .map(baseArcLengthCalculation.evalAt),
          Seq(tangFitOrder, 0, muneFitOrder, kissakeBladeWidthFitOrder),
          bladeWidthVsArcLengthPoints
        )
    }

  val bladeArcLength: Double =
    baseArcLengthCalculation.evalAt(orientedPoints.map(_.x).max)

  val tangArcLength: Option[Double] =
    oTangPoint.map(pt => baseArcLengthCalculation.evalAt(pt.x))

  val tsubaBoundaryArcLengths: Seq[Double] =
    tsubaPoints.map(pt => baseArcLengthCalculation.evalAt(pt.x))

  def rotateMuneModel(rotation: Double): PiecewisePolynomial1DSupport =
    oTangPoint match {
      case Some(tangPoint) =>
        PiecewisePolynomial1DSupport.fitPiecewisePolynomial(
          Seq(Point2D.rotateAboutOrigin(tangPoint.point2d, rotation).x),
          Seq(tangFitOrder, muneFitOrder),
          orientedPoints.map(p =>
            Point2D.rotateAboutOrigin(p.point2d, rotation)
          )
        )
      case None =>
        PiecewisePolynomial1DSupport.fitPiecewisePolynomial(
          tsubaPoints.map(pt =>
            Point2D.rotateAboutOrigin(pt.point2d, rotation).x
          ),
          Seq(tangFitOrder, 0, muneFitOrder),
          orientedPoints.map(p =>
            Point2D.rotateAboutOrigin(p.point2d, rotation)
          )
        )
    }

  def torqueIntegrand(
      fulcrumPosition: Double,
      muneModel: PiecewisePolynomial1DSupport
  ): RealValuedFunction = new RealValuedFunction {
    val arcLength: ArcLengthFromZero = ArcLengthFromZero(muneModel)

    val inverseArcLength: ArcLengthFromZeroInverse =
      ArcLengthFromZeroInverse(arcLength, arcLengthInversionAccuracy)

    val xDiff: RealValuedFunction = AnalyticRealValuedFunction(
      fulcrumPosition - _
    ).composeWith(inverseArcLength)

    val fp: RealValuedFunction =
      muneModel.derivative.composeWith(inverseArcLength)

    val fp2: RealValuedFunction =
      AnalyticRealValuedFunction(Math.pow(_, 2)).composeWith(fp)

    val fpp: RealValuedFunction =
      muneModel.derivative.derivative.composeWith(inverseArcLength)

    val denom: RealValuedFunction =
      AnalyticRealValuedFunction(i => Math.sqrt(1d + i)).composeWith(fp2)

    override def evalAt(s: Double): Double =
      xDiff.evalAt(s) + baseBladeWidthModel
        .evalAt(s) / 2.0 * (xDiff.evalAt(s) * (-fpp
        .evalAt(s) / Math.pow(denom.evalAt(s), 3) + fp2.evalAt(s) * fpp.evalAt(
        s
      ) * (1 - fp.evalAt(s)) / Math.pow(denom.evalAt(s), 5)) - fp.evalAt(
        s
      ) / denom.evalAt(s)) + Math.pow(baseBladeWidthModel.evalAt(s),
                                      2
      ) / 3.0 * (fp
        .evalAt(s) * fpp.evalAt(s) / Math.pow(denom.evalAt(s), 4) - Math.pow(
        fp.evalAt(s),
        3
      ) * fpp.evalAt(s) * (1 - fp.evalAt(s)) / Math.pow(denom.evalAt(s), 6))

  }

  // Post sampling calculations

  private lazy val baseInverseArcLengthModel: ArcLengthFromZeroInverse =
    ArcLengthFromZeroInverse(baseArcLengthCalculation,
                             arcLengthInversionAccuracy
    )

  private lazy val arcLengthToMuneDerivative: RealValuedFunction =
    baseMuneModel.derivative.composeWith(baseInverseArcLengthModel)

  private lazy val arcLengthToMuneDoubleDerivative: RealValuedFunction =
    baseMuneModel.derivative.derivative.composeWith(baseInverseArcLengthModel)

  lazy val arcLengthToMuneCurvature: RealValuedFunction = (s: Double) =>
    Math.abs(arcLengthToMuneDoubleDerivative.evalAt(s)) / Math.pow(
      1d + Math.pow(arcLengthToMuneDerivative.evalAt(s), 2),
      1.5
    )

  lazy val chordalLength: Double = Point2D.distanceBetween(
    orientedPoints.minBy(_.x).point2d,
    orientedPoints.maxBy(_.x).point2d
  )

  // Rotate so blade tip and Mune-side of the Tsuba top-side
  // lie on the x-axis. We then find the maximum of the curve
  // traced by the Mune.
  lazy val sori: Double = {
    val basePoint = oTangPoint.getOrElse(tsubaPoints.maxBy(_.x)).point2d
    val tipPoint  = orientedPoints.maxBy(_.x).point2d

    val rotation: Double =
      -Math.atan2(tipPoint.y - basePoint.y, tipPoint.x - basePoint.x)

    val rotatedModel = rotateMuneModel(rotation)

    rotatedModel.evalAt(
      rotatedModel.derivative.solveFor(0, (tipPoint.x + basePoint.x) / 2.0)
    )
  }
}
