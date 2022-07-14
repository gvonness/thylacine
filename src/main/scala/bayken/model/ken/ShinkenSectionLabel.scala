package ai.entrolution
package bayken.model.ken

import enumeratum.EnumEntry._
import enumeratum._

sealed trait ShinkenSectionLabel extends EnumEntry with Snakecase

object ShinkenSectionLabel extends Enum[ShinkenSectionLabel] {
  val values: IndexedSeq[ShinkenSectionLabel] = findValues
  case object Tsuka   extends ShinkenSectionLabel
  case object Tang    extends ShinkenSectionLabel
  case object Tsuba   extends ShinkenSectionLabel
  case object Habaki  extends ShinkenSectionLabel
  case object Blade   extends ShinkenSectionLabel
  case object Kissake extends ShinkenSectionLabel
}
