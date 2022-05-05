package ai.entrolution
package thylacine.config

case class SlqConfig(
    poolSize: Int,
    abscissaNumber: Int,
    domainScalingIncrement: Double,
    targetAcceptanceProbability: Double,
    sampleParallelism: Int
)
