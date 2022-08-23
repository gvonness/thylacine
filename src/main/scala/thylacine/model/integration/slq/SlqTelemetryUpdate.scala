package ai.entrolution
package thylacine.model.integration.slq

case class SlqTelemetryUpdate(
    negEntropyAvg: Double,
    logPdf: Double,
    samplePoolMinimumLogPdf: Double,
    domainVolumeScaling: Double,
    acceptancesSinceDomainRebuild: Int,
    samplePoolSize: Int,
    domainCubeCount: Int,
    iterationCount: Int
)
