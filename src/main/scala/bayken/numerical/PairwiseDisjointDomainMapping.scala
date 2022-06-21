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

import bayken.numerical.Interval1DCollection.NontrivialInterval1DCollection

case class PairwiseDisjointDomainMapping[T](
    domainMapping: Map[NontrivialInterval1DCollection, T]
) {

  val mergedDomainIntervals: NontrivialInterval1DCollection =
    domainMapping.keySet.reduce(_ union _)

  val isPairwiseDisjoint = mergedDomainIntervals.isPairwiseDisjoint

  assert(
    domainMapping.map(_._1.sets.size).sum == mergedDomainIntervals.sets.size
  )
  assert(mergedDomainIntervals.isPairwiseDisjoint)

  def retrieveMappedEntity(x: Double): Option[T] =
    domainMapping.find(_._1.contains(x)).map(_._2)

  def intersectWith(interval1D: Interval1D): PairwiseDisjointDomainMapping[T] =
    PairwiseDisjointDomainMapping(domainMapping.collect { d =>
      d._1.intersectWith(interval1D) match {
        case res @ NontrivialInterval1DCollection(_) => res -> d._2
      }
    })
}
