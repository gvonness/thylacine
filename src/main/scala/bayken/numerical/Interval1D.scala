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

import bayken.numerical.Interval1DCollection.{
  NontrivialInterval1DCollection,
  TrivialInterval1DCollection
}

// The interval model can accommodate infinite intervals and compliments. I stopped short
// of modelling a full sigma-algebra as the below should be sufficient for defining piecewise
// functions over the real line

sealed trait Interval1D {
  def contains(value: Double): Boolean
  def intersectWith(otherInterval: Interval1D): Interval1D
  def compliment: Interval1DCollection
}

object Interval1D {

  case object NullInterval1D extends Interval1D {
    override def contains(value: Double): Boolean = false

    override def intersectWith(otherInterval: Interval1D): Interval1D =
      NullInterval1D

    override def compliment: Interval1DCollection =
      NontrivialInterval1DCollection(Set(RealLine))
  }

  case class SinglePointInterval(pointValue: Double) extends Interval1D {

    override def contains(value: Double): Boolean =
      pointValue == value

    override def intersectWith(otherInterval: Interval1D): Interval1D =
      if (otherInterval.contains(pointValue)) {
        this
      } else {
        NullInterval1D
      }

    override def compliment: Interval1DCollection =
      NontrivialInterval1DCollection(
        Set(NegativeUnbounded(OpenBoundary(pointValue)),
            PositiveUnbounded(OpenBoundary(pointValue))
        )
      )
  }

  case object RealLine extends Interval1D {
    override def contains(value: Double): Boolean = true

    override def intersectWith(otherInterval: Interval1D): Interval1D =
      otherInterval

    override def compliment: Interval1DCollection =
      TrivialInterval1DCollection
  }

  case class PositiveUnbounded(boundary: IntervalBoundary) extends Interval1D {

    override def contains(value: Double): Boolean =
      boundary match {
        case OpenBoundary(v)   => v < value
        case ClosedBoundary(v) => v <= value
      }

    override def intersectWith(otherInterval: Interval1D): Interval1D =
      otherInterval match {
        case OrderedBoundedInterval1D(lb, ub) =>
          if (ub.boundaryValue < boundary.boundaryValue)
            NullInterval1D
          else if (boundary.boundaryValue < lb.boundaryValue)
            otherInterval
          else if (boundary.boundaryValue == ub.boundaryValue)
            (ub, boundary) match {
              case (ClosedBoundary(_), ClosedBoundary(_)) =>
                SinglePointInterval(ub.boundaryValue)
              case _ =>
                NullInterval1D
            }
          else if (boundary.boundaryValue == lb.boundaryValue)
            (lb, boundary) match {
              case (ClosedBoundary(_), OpenBoundary(_)) =>
                OrderedBoundedInterval1D(boundary, ub)
              case _ =>
                otherInterval
            }
          else
            OrderedBoundedInterval1D(boundary, ub)
        case PositiveUnbounded(v) =>
          if (v.boundaryValue == boundary.boundaryValue)
            (boundary, v) match {
              case (_, OpenBoundary(_)) =>
                otherInterval
              case _ =>
                this
            }
          else if (v.boundaryValue < boundary.boundaryValue)
            this
          else
            otherInterval
        case NegativeUnbounded(v) =>
          if (v.boundaryValue < boundary.boundaryValue)
            NullInterval1D
          else if (v.boundaryValue > boundary.boundaryValue)
            OrderedBoundedInterval1D(boundary, v)
          else
            (boundary, v) match {
              case (ClosedBoundary(_), ClosedBoundary(_)) =>
                SinglePointInterval(v.boundaryValue)
              case _ =>
                NullInterval1D
            }
        case i =>
          i.intersectWith(this)
      }

    override def compliment: Interval1DCollection =
      boundary match {
        case ClosedBoundary(v) =>
          NontrivialInterval1DCollection(
            Set(NegativeUnbounded(OpenBoundary(v)))
          )
        case _ =>
          NontrivialInterval1DCollection(
            Set(NegativeUnbounded(ClosedBoundary(boundary.boundaryValue)))
          )
      }
  }

  case class NegativeUnbounded(boundary: IntervalBoundary) extends Interval1D {

    override def contains(value: Double): Boolean =
      boundary match {
        case OpenBoundary(v)   => v > value
        case ClosedBoundary(v) => v >= value
      }

    override def intersectWith(otherInterval: Interval1D): Interval1D =
      otherInterval match {
        case OrderedBoundedInterval1D(lb, ub) =>
          if (lb.boundaryValue > boundary.boundaryValue)
            NullInterval1D
          else if (boundary.boundaryValue > ub.boundaryValue)
            otherInterval
          else if (boundary.boundaryValue == lb.boundaryValue)
            (lb, boundary) match {
              case (ClosedBoundary(_), ClosedBoundary(_)) =>
                SinglePointInterval(lb.boundaryValue)
              case _ =>
                NullInterval1D
            }
          else if (boundary.boundaryValue == ub.boundaryValue)
            (ub, boundary) match {
              case (ClosedBoundary(_), OpenBoundary(_)) =>
                OrderedBoundedInterval1D(lb, boundary)
              case _ =>
                otherInterval
            }
          else
            OrderedBoundedInterval1D(lb, boundary)
        case NegativeUnbounded(v) =>
          if (v.boundaryValue == boundary.boundaryValue)
            (boundary, v) match {
              case (_, OpenBoundary(_)) =>
                otherInterval
              case _ =>
                this
            }
          else if (v.boundaryValue > boundary.boundaryValue)
            this
          else
            otherInterval
        case i =>
          i.intersectWith(this)
      }

    override def compliment: Interval1DCollection =
      boundary match {
        case ClosedBoundary(v) =>
          NontrivialInterval1DCollection(
            Set(PositiveUnbounded(OpenBoundary(v)))
          )
        case _ =>
          NontrivialInterval1DCollection(
            Set(PositiveUnbounded(ClosedBoundary(boundary.boundaryValue)))
          )
      }
  }

  case class OrderedBoundedInterval1D(
      lowerBoundary: IntervalBoundary,
      upperBoundary: IntervalBoundary
  ) extends Interval1D {
    assert(lowerBoundary.boundaryValue < upperBoundary.boundaryValue)

    override def contains(value: Double): Boolean =
      (lowerBoundary, upperBoundary) match {
        case (OpenBoundary(v1), OpenBoundary(v2)) =>
          v1 < value && value < v2
        case (ClosedBoundary(v1), ClosedBoundary(v2)) =>
          v1 <= value && value <= v2
        case (OpenBoundary(v1), ClosedBoundary(v2)) =>
          v1 < value && value <= v2
        case (ClosedBoundary(v1), OpenBoundary(v2)) =>
          v1 <= value && value < v2
      }

    override def intersectWith(otherInterval: Interval1D): Interval1D =
      otherInterval match {
        case OrderedBoundedInterval1D(lb, ub) =>
          if (
            lb.boundaryValue > upperBoundary.boundaryValue || ub.boundaryValue < lowerBoundary.boundaryValue
          )
            NullInterval1D
          else if (
            lowerBoundary.boundaryValue < lb.boundaryValue && upperBoundary.boundaryValue > ub.boundaryValue
          )
            otherInterval
          else if (
            lowerBoundary.boundaryValue > lb.boundaryValue && upperBoundary.boundaryValue < ub.boundaryValue
          )
            this
          else if (
            lowerBoundary.boundaryValue < lb.boundaryValue && upperBoundary.boundaryValue < ub.boundaryValue && lb.boundaryValue < upperBoundary.boundaryValue
          )
            OrderedBoundedInterval1D(lb, upperBoundary)
          else if (
            lowerBoundary.boundaryValue > lb.boundaryValue && upperBoundary.boundaryValue > ub.boundaryValue && ub.boundaryValue > lowerBoundary.boundaryValue
          )
            OrderedBoundedInterval1D(lowerBoundary, ub)
          else if (
            lb.boundaryValue == lowerBoundary.boundaryValue && ub.boundaryValue == upperBoundary.boundaryValue
          ) {
            val newLowerBoundary =
              (lowerBoundary, lb) match {
                case (_, OpenBoundary(_)) =>
                  lb
                case _ =>
                  lowerBoundary
              }

            val newUpperBoundary =
              (upperBoundary, ub) match {
                case (_, OpenBoundary(_)) =>
                  ub
                case _ =>
                  upperBoundary
              }

            OrderedBoundedInterval1D(newLowerBoundary, newUpperBoundary)
          } else if (lb.boundaryValue == lowerBoundary.boundaryValue)
            (lowerBoundary, lb) match {
              case (ClosedBoundary(_), ClosedBoundary(_)) =>
                SinglePointInterval(lowerBoundary.boundaryValue)
              case _ =>
                NullInterval1D
            }
          else
            (upperBoundary, ub) match {
              case (ClosedBoundary(_), ClosedBoundary(_)) =>
                SinglePointInterval(upperBoundary.boundaryValue)
              case _ =>
                NullInterval1D
            }
        case i =>
          i.intersectWith(this)
      }

    override def compliment: Interval1DCollection =
      PositiveUnbounded(lowerBoundary).compliment.union(
        NegativeUnbounded(upperBoundary).compliment
      )
  }

  object OrderedBoundedInterval1D {

    def openInterval(x0: Double, x1: Double): Interval1D =
      if (x0 < x1) {
        OrderedBoundedInterval1D(OpenBoundary(x0), OpenBoundary(x1))
      } else if (x1 < x0) {
        OrderedBoundedInterval1D(OpenBoundary(x1), OpenBoundary(x0))
      } else {
        NullInterval1D
      }

    def closedInterval(x0: Double, x1: Double): Interval1D =
      if (x0 < x1) {
        OrderedBoundedInterval1D(ClosedBoundary(x0), ClosedBoundary(x1))
      } else if (x1 < x0) {
        OrderedBoundedInterval1D(ClosedBoundary(x1), ClosedBoundary(x0))
      } else {
        SinglePointInterval(x1)
      }
  }
}
