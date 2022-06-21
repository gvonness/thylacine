package ai.entrolution
package bayken.config.visualisation

case class AbscissaConfig(size: Int, min: Double, max: Double) {
  assert(max > min)
}
