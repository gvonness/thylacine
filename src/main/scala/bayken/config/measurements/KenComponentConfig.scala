package ai.entrolution
package bayken.config.measurements

case class KenComponentConfig(
    tsuba: TsubaConfig,
    habaraki: HabarakiConfig,
    seppa: Option[SeppaConfig],
    fuchi: FuchiConfig,
    tsuka: TsukaConfig,
    blade: BladeConfig
)
