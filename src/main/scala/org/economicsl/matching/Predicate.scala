package org.economicsl.matching


/** A mixin trait that uses a boolean function to express preferences over a particular `T`. */
trait Predicate[-T] {

  /** Boolean function used to determine whether some `T` is acceptable.
    *
    * @return a boolean function that returns `true` if the `T` is acceptable and `false` otherwise.
    */
  def isAcceptable: T => Boolean

}
