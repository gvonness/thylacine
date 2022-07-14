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

import bayken.config.inference.MassInferenceConfig
import bayken.config.measurements.BackEdgePointConfig
import bayken.model.ken.ShinkenGeometry.getSortedAndJoinedBladeSections
import bayken.model.ken.ShinkenSectionLabel._
import bayken.numerical.Interval1D.OrderedBoundedInterval1D
import bayken.numerical.Interval1DCollection.NontrivialInterval1DCollection
import bayken.numerical.Polynomial1D.NonTrivialPolynomial
import bayken.numerical._
import bayken.numerical.arclength.{ArcLengthFromZero, ArcLengthFromZeroInverse}

// Overrides are entry points for priors in non-analytic inferences
case class ShinkenGeometry(
    rawMunePoints: List[BackEdgePointConfig],
    inferenceConfig: MassInferenceConfig,
    munePointXOverride: Option[List[Double]],
    munePointYOverride: Option[List[Double]],
    habaOverride: Option[List[Double]]
) {

  /*
   * - - -- --- ----- -------- -------------
   * Blade point measurements - Orientation
   * and validation
   * - - -- --- ----- -------- -------------
   */

  private lazy val munePoints: List[BackEdgePointConfig] = {
    val xProcessedPoints: List[BackEdgePointConfig] = munePointXOverride match {
      case Some(xOverride) =>
        assert(xOverride.size == rawMunePoints.size)
        rawMunePoints.zip(xOverride).map(pts => pts._1.copy(x = pts._2))
      case _ =>
        rawMunePoints
    }

    val yProcessedPoints: List[BackEdgePointConfig] = munePointYOverride match {
      case Some(yOverride) =>
        assert(yOverride.size == xProcessedPoints.size)
        xProcessedPoints.zip(yOverride).map(pts => pts._1.copy(y = pts._2))
      case _ =>
        xProcessedPoints
    }

    val habaProcessedPoints: List[BackEdgePointConfig] = habaOverride match {
      case Some(depthOverride) =>
        val (pointsWithWidths, pointsWithoutWidths) =
          xProcessedPoints.partition(p => p.haba.isDefined)
        assert(pointsWithWidths.size == depthOverride.size)
        pointsWithWidths
          .zip(depthOverride)
          .map(pts => pts._1.copy(haba = Some(pts._2))) ++ pointsWithoutWidths
      case _ =>
        yProcessedPoints
    }

    // Check to see if point at the base of the Tsuka is closest to origin.
    // Reflect the points about the x-axis if this is not the case
    if (
      habaProcessedPoints
        .filter(p => p.shinkenSection == Tang || p.shinkenSection == Tsuka)
        .map(_.x)
        .min < habaProcessedPoints.filter(_.shinkenSection == Blade).map(_.x).max
    ) {
      habaProcessedPoints
    } else {
      habaProcessedPoints.map(p => p.copy(x = -p.x))
    }
  }

  assert(munePoints.count(_.kissakeSaki) == 1)
  assert(munePoints.count(_.muneMachi) == 1)

  private val bladeSectionsMeasured: Set[ShinkenSectionLabel] =
    munePoints.map(_.shinkenSection).toSet

  assert(
    bladeSectionsMeasured.contains(Tang) || bladeSectionsMeasured.contains(
      Tsuka
    )
  )
  assert(bladeSectionsMeasured.contains(Blade))
  assert(bladeSectionsMeasured.contains(Habaki))
  assert(bladeSectionsMeasured.contains(Tsuba))
  assert(bladeSectionsMeasured.contains(Kissake))

  private def rotatePointsAboutOrigin(
      input: List[BackEdgePointConfig],
      rotation: Double
  ): List[BackEdgePointConfig] =
    input.map { i =>
      val newPoint = Point2D.rotateAboutOrigin(i.point2d, rotation)
      i.copy(x = newPoint.x, y = newPoint.y)
    }

  // Defining the Mune-model
  // - - -- --- ----- --------
  // * Mune-side of the Habaki is parallel to the x-axis
  // * Constructed so that only the mune blade polynomial
  //   needs to be specified for the coordinate transformation
  //   model (everywhere else is a 0-valued constant)
  // * See writeup for discussion on coordinates

  lazy val orientedPoints: List[BackEdgePointConfig] = {
    val habakiPoints = munePoints
      .filter(mp => mp.shinkenSection == Habaki)
    val bladeBasePoint = munePoints
      .filter(bp => bp.shinkenSection == Blade)
      .minBy(_.x)

    val habakiMax = habakiPoints.maxBy(_.x).point2d
    val habakiMin = habakiPoints.minBy(_.x).point2d

    val rotation: Double = Math.atan2(habakiMax.y - habakiMin.y, habakiMax.x - habakiMin.x)

    val rotatedPoints = rotatePointsAboutOrigin(munePoints, -rotation)

    val origin = rotatedPoints.minBy(_.x)

    rotatedPoints.map(p => p.copy(x = Math.abs(p.x - origin.x), y = p.y - bladeBasePoint.y))
  }

  lazy val kissakeSakiPoint: BackEdgePointConfig =
    orientedPoints.find(_.kissakeSaki).get

  lazy val muneMachiPoint: BackEdgePointConfig =
    orientedPoints.find(_.muneMachi).get

  /*
   * - - -- --- ----- -------- -------------
   * Blade curve creation
   * - - -- --- ----- -------- -------------
   */

  private def generateModelCurve(
      points: List[BackEdgePointConfig],
      bladeSectionDerivativeZero: Boolean = false
  ): PiecewisePolynomial1DSupport = {
    val sortedAndJoinedBladeSections: Seq[ShinkenSectionModel] =
      getSortedAndJoinedBladeSections(
        inferenceConfig,
        points
      )

    def polynomialFor(section: ShinkenSectionModel): NonTrivialPolynomial =
      section.shinkenSection match {
        case Blade if bladeSectionDerivativeZero => section.getFitPolynomial(List(Point2D(section.lowerBound, 0)))
        case _                                   => section.fitPolynomial
      }

    PiecewisePolynomial1DSupport(
      PairwiseDisjointDomainMapping(
        (sortedAndJoinedBladeSections.init.map { section =>
          (NontrivialInterval1DCollection(
             Set(
               OrderedBoundedInterval1D(ClosedBoundary(section.lowerBound), OpenBoundary(section.upperBound))
             )
           ),
           polynomialFor(section)
          )
        } :+ (NontrivialInterval1DCollection(
          Set(
            OrderedBoundedInterval1D(ClosedBoundary(sortedAndJoinedBladeSections.last.lowerBound),
                                     ClosedBoundary(sortedAndJoinedBladeSections.last.upperBound)
            )
          )
        ), polynomialFor(sortedAndJoinedBladeSections.last))).toMap
      )
    )
  }

  // Blade mune defines a non-trivial polynomial from habaki
  // border to the tip (actually to infinity). From the habaki
  // border to the base of the handle (actually to negative
  // infinity), the model is just a zero-valued constant function.
  //
  // This model defines the coordinates for the actual torque
  // integration for the mass inference.
  lazy val coordinateModel: PiecewisePolynomial1DSupport =
    generateModelCurve(
      orientedPoints.flatMap { pt =>
        pt.shinkenSection match {
          case Kissake => Some(pt.copy(shinkenSection = Blade))
          case Blade   => Some(pt)
          case _       => None
        }
      },
      bladeSectionDerivativeZero = true
    )

  val coordsToArcLength: ArcLengthFromZero = ArcLengthFromZero(coordinateModel)

  val arcLengthToCoords: ArcLengthFromZeroInverse =
    ArcLengthFromZeroInverse(coordsToArcLength, inferenceConfig.inverseArcLengthConvergenceTolerance)

  val f: RealValuedFunction =
    coordinateModel

  val fp: RealValuedFunction =
    coordinateModel.derivative

  val fpp: RealValuedFunction =
    coordinateModel.derivative.derivative

  // Model defining the points at which the shinken could be
  // supported by mass measurement holders (Mune, Mune-side
  // of the habaki and mune-side of the Tsuka).
  //
  // This model is used to determine the rotation of the blade
  // and precise placement of the holders during the actual
  // experiments
  val backEdgeModel: PiecewisePolynomial1DSupport =
    generateModelCurve(
      orientedPoints.map { pt =>
        pt.shinkenSection match {
          case Kissake => pt.copy(shinkenSection = Blade)
          case _       => pt
        }
      },
      bladeSectionDerivativeZero = true
    )

  // Model to generate the lower and upper bounds for the Haba integration. Note
  // these are functions of the originating cartesian coordinates
  val (lowerHabaModel: RealValuedFunction, upperHabaModel: RealValuedFunction) = {
    val lowerHabaBaseModel = generateModelCurve(
      orientedPoints.filter(pt => pt.shinkenSection != Blade && pt.shinkenSection != Kissake)
    )

    val upperHabaBaseModel = generateModelCurve(orientedPoints.flatMap { pt =>
      pt.haba.map { h =>
        pt.copy(y = h + lowerHabaBaseModel.evalAt(pt.x))
      }
    })

    (lowerHabaBaseModel, upperHabaBaseModel)
  }

  def torqueIntegrand(
      fulcrumPosition: Point2D,
      rotation: Double
  ): RealValuedFunction = new RealValuedFunction {

    val cosT: Double = Math.cos(rotation)
    val sinT: Double = Math.sin(rotation)

    override def evalAt(s: Double): Double = {
      val xs   = arcLengthToCoords.evalAt(s)
      val fs   = f.evalAt(xs)
      val fps  = fp.evalAt(xs)
      val fp2s = Math.pow(fps, 2)
      val fpps = fpp.evalAt(xs)

      val hu      = upperHabaModel.evalAt(xs)
      val hl      = lowerHabaModel.evalAt(xs)
      val hu2diff = Math.pow(hu, 2) - Math.pow(hl, 2)
      val hu3diff = Math.pow(hu, 3) - Math.pow(hl, 3)

      val A     = cosT * (xs - fulcrumPosition.x) - sinT * (fs - fulcrumPosition.y)
      val B     = cosT * fps + sinT
      val C     = 1 + fp2s
      val sqrtC = Math.sqrt(C)
      val C2    = Math.pow(C, 2)
      val D     = fp2s * fpps * (1 - fps)

      A * (hu - hl) + hu2diff / (2 * sqrtC) * (D * A / C2 - B - fpps * A / C) + hu3diff / (3 * C2) * (fpps * B - D * B / C)
    }
  }

  // Reconstruct mass-per-length from parametrisation

  val massInferencePolesAndWeights: List[(Double, Double)] = {
    val sortedAndJoinedBladeSections =
      getSortedAndJoinedBladeSections(
        inferenceConfig,
        orientedPoints,
        p => p.copy(x = coordsToArcLength.evalAt(p.x))
      )

    sortedAndJoinedBladeSections.map { bsc =>
      LegendreQuadrature(bsc.quadratureSize).getPolesAndWeights(bsc.lowerBound, bsc.upperBound)
    }.reduce(_ ++ _)
  }

  // Used to constrain mass inference to be continuous
  // around the Kissake
  lazy val quadratureKissakeBoundaryIsolated: List[Double] = {
    val sortedAndJoinedBladeSections =
      getSortedAndJoinedBladeSections(
        inferenceConfig,
        orientedPoints,
        p => p.copy(x = coordsToArcLength.evalAt(p.x))
      )

    sortedAndJoinedBladeSections.map { bsc =>
      bsc.shinkenSection match {
        case Blade =>
          List.fill(bsc.quadratureSize - 1)(0d) :+ 1d
        case Kissake =>
          -1d +: List.fill(bsc.quadratureSize - 1)(0d)
        case _ =>
          List.fill(bsc.quadratureSize)(0d)
      }
    }.reduce(_ ++ _)
  }

  lazy val bladeSectionOrderedPartitions: Seq[(ShinkenSectionLabel, List[BackEdgePointConfig])] =
    orientedPoints
      .groupBy(_.shinkenSection)
      .toList
      .sortBy(_._2.map(_.x).min)

  lazy val inferenceQuadratureOrders: Seq[Int] =
    bladeSectionOrderedPartitions.flatMap { bSec =>
      inferenceConfig.bladeSectionParameters
        .find(b => b.label == bSec._1)
        .map(_.quadratureSize)
    }

  lazy val inferencePolynomialOrders: Seq[Int] =
    bladeSectionOrderedPartitions.flatMap { bSec =>
      inferenceConfig.bladeSectionParameters
        .find(b => b.label == bSec._1)
        .map(_.massPolynomialFitOrder)
    }

  def mapMassCoeffiencetsIntoQuadratureValues(
      coefficients: Vector[Double]
  ): List[Double] = {
    val sortedAndJoinedBladeSections =
      getSortedAndJoinedBladeSections(
        inferenceConfig,
        orientedPoints,
        p => p.copy(x = coordsToArcLength.evalAt(p.x))
      )

    val sectionPolynomials = bladeSectionOrderedPartitions.map(_._1).zip {
      inferenceQuadratureOrders
        .map(_ + 1)
        .foldLeft(List[NonTrivialPolynomial](), coefficients) { (i, j) =>
          val splitCoefficients = i._2.splitAt(j)
          (i._1 :+ NonTrivialPolynomial(splitCoefficients._1.toList), splitCoefficients._2)
        }
        ._1
    }

    // Probably not necessary, as there is a canonical ordering held across
    // the calculations
    val orderedPolynomials = sortedAndJoinedBladeSections.flatMap { bsc =>
      sectionPolynomials.find(_._1 == bsc.shinkenSection).map(_._2)
    }

    val massPolynomial = PiecewisePolynomial1DSupport.constructPiecewisePolynomial(
      boundaries = sortedAndJoinedBladeSections.init
        .map(_.upperBound),
      nonTrivialPolynomials = orderedPolynomials
    )

    massInferencePolesAndWeights.map(pw => massPolynomial.evalAt(pw._1))
  }

  // Post sampling calculations
  private lazy val arcLengthToMuneDerivative: RealValuedFunction =
    coordinateModel.derivative.composeWith(arcLengthToCoords)

  private lazy val arcLengthToMuneDoubleDerivative: RealValuedFunction =
    coordinateModel.derivative.derivative.composeWith(arcLengthToCoords)

  lazy val arcLengthToMuneCurvature: RealValuedFunction = (s: Double) =>
    Math.abs(arcLengthToMuneDoubleDerivative.evalAt(s)) / Math.pow(
      1d + Math.pow(arcLengthToMuneDerivative.evalAt(s), 2),
      1.5
    )

  lazy val nagasa: Double = Point2D.distanceBetween(muneMachiPoint.point2d, kissakeSakiPoint.point2d)

  // Rotate so blade tip and Mune-side of the Tsuba top-side
  // lie on the x-axis. We then find the maximum of the curve
  // traced by the Mune to determine the Sori.
  lazy val sori: Double = {
    val rotation = Math.atan2(kissakeSakiPoint.y - muneMachiPoint.y, kissakeSakiPoint.x - muneMachiPoint.x)

    val xCoord = coordinateModel.derivative.solveFor(
      (kissakeSakiPoint.y - muneMachiPoint.y) / (kissakeSakiPoint.x - muneMachiPoint.x),
      (kissakeSakiPoint.x + muneMachiPoint.x) / 2.0
    )
    val yCood = coordinateModel.evalAt(xCoord)

    Point2D.rotateAboutOrigin(Point2D(xCoord, yCood), -rotation).y
  }
}

object ShinkenGeometry {

  def getSortedAndJoinedBladeSections(
      inferenceConfig: MassInferenceConfig,
      points: List[BackEdgePointConfig],
      xRemapping: Point2D => Point2D = p => p
  ): List[ShinkenSectionModel] = {
    val sortedBladeSections =
      points
        .groupBy(_.shinkenSection)
        .flatMap { ps =>
          inferenceConfig.bladeSectionParameters.find(_.label == ps._1).map { bsc =>
            ShinkenSectionModel(
              shinkenSection = ps._1,
              points = ps._2
                .map(_.point2d)
                .map(xRemapping)
                .toSet,
              quadratureSize = bsc.quadratureSize
            )
          }
        }
        .toList
        .sortBy(_.lowerBound)

    // Ensure that blade section boundaries coincide
    sortedBladeSections.tail.foldLeft(List(sortedBladeSections.head)) { (i, j) =>
      i.init ::: {
        val newBoundary = (i.last.upperBound + j.lowerBound) / 2.0
        List(i.last.copy(upperBound = newBoundary), j.copy(lowerBound = newBoundary))
      }
    }
  }
}
