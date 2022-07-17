/*
 * Copyright 2020-2022 Greg von Nessi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.entrolution
package bayken.numerical

import bayken.numerical.Interval1D.NullInterval1D

sealed trait Interval1DCollection {
  def union(other: Interval1DCollection): Interval1DCollection
  def intersectWith(interval: Interval1D): Interval1DCollection
  def contains(x: Double): Boolean
  def isPairwiseDisjoint: Boolean
}

object Interval1DCollection {

  case class NontrivialInterval1DCollection(sets: Set[Interval1D]) extends Interval1DCollection {
    assert(sets.exists {
      case NullInterval1D => false
      case _              => true
    })

    override def union(
        other: Interval1DCollection
    ): NontrivialInterval1DCollection =
      other match {
        case NontrivialInterval1DCollection(otherSets) =>
          NontrivialInterval1DCollection(sets ++ otherSets)
        case TrivialInterval1DCollection =>
          this
      }

    override def intersectWith(interval: Interval1D): Interval1DCollection = {
      val newIntervals =
        sets.map(_.intersectWith(interval)).filter(_ != NullInterval1D)

      if (newIntervals.nonEmpty) {
        NontrivialInterval1DCollection(newIntervals)
      } else {
        TrivialInterval1DCollection
      }
    }

    override def contains(x: Double): Boolean =
      sets.exists(_.contains(x))

    override lazy val isPairwiseDisjoint: Boolean = {
      if (sets.size > 1) {
        val setList = sets.toList
        setList
          .foldLeft((true, setList.tail)) { (i, j) =>
            if (!i._1 || i._2.isEmpty) {
              i
            } else {
              (!i._2.exists(_.intersectWith(j) != NullInterval1D), i._2.tail)
            }
          }
          ._1
      } else {
        true
      }
    }
  }

  case object TrivialInterval1DCollection extends Interval1DCollection {

    override def union(other: Interval1DCollection): Interval1DCollection =
      other

    override def intersectWith(interval: Interval1D): Interval1DCollection =
      this

    override def contains(x: Double): Boolean = false

    override val isPairwiseDisjoint: Boolean = true
  }
}
