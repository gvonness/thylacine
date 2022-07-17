package ai.entrolution
package bayken.config.visualisation

case class ProbabalisticVisualisationConfig(
    sampleCount: Int,
    graphLineIncrement: Double,
    graphKernelVariance: Double,
    numXPlotPoints: Int,
    numYPlotPoints: Int
)
