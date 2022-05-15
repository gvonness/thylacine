package ai.entrolution
package bayken.config

case class KenConfig(
    label: String,
    recalculateAnalyticOnNextRun: Boolean,
    measurements: KenMeasurements,
    inferenceParameters: Option[InferenceConfig],
    visualisationParameters: Option[VisualisationConfig]
)
