package ai.entrolution
package bayken.config.measurements

case class BladeConfig(
    mass: Option[Double],
    geometry: List[MunePointConfig]
) {
  lazy val hasTangPoint: Boolean = geometry.exists(_.tangBoundary)
}
