package ai.entrolution
package bayken.config

case class AbscissaConfig(size: Int, min: Double, max: Double) {
  assert(max > min)
}
