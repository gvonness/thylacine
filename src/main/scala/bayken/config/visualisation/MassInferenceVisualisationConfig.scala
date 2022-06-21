package ai.entrolution
package bayken.config.visualisation

case class MassInferenceVisualisationConfig(
    sampleCount: Int,
    graphLineIncrement: Double,
    graphKernelVariance: Double,
    xAbscissa: AbscissaConfig,
    yAbscissa: AbscissaConfig
)
