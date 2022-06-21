package ai.entrolution
package bayken.config.measurements

import bayken.numerical.Point2D

case class MunePointConfig(
    x: Double,
    y: Double,
    depth: Option[Double],
    width: Option[Double],
    kissakeBoundary: Boolean = false,
    tangBoundary: Boolean = false,
    tsubaBoundary: Boolean = false
) {
  lazy val point2d: Point2D = Point2D(x, y)
}
