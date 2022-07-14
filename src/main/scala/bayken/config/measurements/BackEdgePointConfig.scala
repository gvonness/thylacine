package ai.entrolution
package bayken.config.measurements

import bayken.model.ken.ShinkenSectionLabel
import bayken.numerical.Point2D

// Breaking this out into a sealed trait would only obfuscate
// the connection to section-specific inference parameters
case class BackEdgePointConfig(
    x: Double,
    y: Double,
    shinkenSection: ShinkenSectionLabel,
    kasane: Option[Double],
    haba: Option[Double],
    shinogiHaba: Option[Double],
    muneMachi: Boolean = false,
    kissakeSaki: Boolean = false,
    nakagoJiri: Boolean = false
) {
  lazy val point2d: Point2D = Point2D(x, y)
}
