package ai.entrolution
package thylacine.model.core

/** We use the type system to ensure the right structure
  * of the Bayesian Graph components. but do not use it in
  * validating the mathematics of the underpinning
  * computations. Doing so would make the typing model
  * significantly more complicated to get some relative
  * simple sanity checks in place.
  *
  * Instead we create a flexible validation framework that
  * will be enacted at the creation of any Bayesian graphical
  * network. Obviously, this won't get us compile time
  * guarantees, but violations will fail the model quickly
  * at runtime
  */
trait CanValidate[+T <: CanValidate[_]] {
  def validated: Boolean
  def getValidated: T
}
