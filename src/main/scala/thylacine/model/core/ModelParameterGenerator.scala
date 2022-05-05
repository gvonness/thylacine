package ai.entrolution
package thylacine.model.core

import thylacine.model.core.GenericIdentifier._

trait ModelParameterGenerator {
  // Model parameter dimensions need to be disjoint and uniquely
  // identifiable. We could enforce this via typing, but it's
  // preferred to relegate typing to enforce the Bayesian model
  // structure and not get entangled with the underpinning calculations
  def identifier: ModelParameterIdentifier

  def generatorDimension: Int
}
