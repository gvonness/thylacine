package ai.entrolution
package thylacine.config

case class HmcmcConfig(
    stepsBetweenSamples: Int,
    stepsInDynamicsSimulation: Int,
    warmupStepCount: Int,
    dynamicsSimulationStepSize: Double
)
