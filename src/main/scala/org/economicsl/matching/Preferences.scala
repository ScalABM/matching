package org.economicsl.matching


/** A mixin trait that uses a total `Ordering` to express preferences over a particular type of `Tradable`.
  *
  * @tparam A the type over which the `Ordering` is defined.
  * @note any `Ordering` implies a `max` operator that can be used as an `operator` to compare two `Tradable` instances.
  */
trait Preferences[A] {

  /** An `Ordering` defined over a particular type of `A`. */
  def ordering: Ordering[A]

}
