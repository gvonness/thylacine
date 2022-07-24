package ai.entrolution
package bayken.config.inference

import thylacine.config.{HmcmcConfig, HookeAndJeevesConfig}

case class MassInferenceConfig(
    gaussianProcessCoefficient: Double,
    bladeSectionParameters: List[ShinkenSectionConfig],
    rotationalFitTolerance: Double,
    inverseArcLengthConvergenceTolerance: Double,
    hookesAndJeevesParameters: HookeAndJeevesConfig,
    hmcmcParameters: HmcmcConfig
) {
  assert(bladeSectionParameters.size == bladeSectionParameters.map(_.label).toSet.size)
}
