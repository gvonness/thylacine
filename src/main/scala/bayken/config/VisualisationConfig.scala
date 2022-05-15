package ai.entrolution
package bayken.config

case class VisualisationConfig(
    sampleCount: Int,
    graphLineIncrement: Double,
    graphKernelVariance: Double,
    xAbscissa: AbscissaConfig,
    yAbscissa: AbscissaConfig
)
