/*
Copyright 2018 EconomicSL

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
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


object Preferences {

  /** Create an `Ordering` over type `A` based on the order in which elements of type `A` appear in a given list.
    *
    * @tparam A the type over which the `Ordering` should be defined.
    * @return an `Ordering[A]` based on the order in which elements appear in a given `List[A]`.
    */
  def fromList[A]: List[A] => Ordering[A] = {
    list => Ordering.by[A, Int](a => list.reverse.indexOf(a)).reverse
  }

}
