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
package org.economicsl.matching.one2one

import org.economicsl.matching.{Preferences, Proposer}

/** Class used to represent a matching.
  *
  * @param matches
  * @tparam A
  * @tparam B
  */
case class Matching[A <: Preferences[B], B <: Proposer with Preferences[A]](matches: Map[A, B]) {

  lazy val invertedMatches: Map[B, A] = matches.map(_.swap)

  def get(a: A): Option[B] = {
    matches.get(a)
  }

  def get(b: B): Option[A] = {
    invertedMatches.get(b)
  }

  /** Test whether the matching can be blocked by a given pair.
    *
    * @param kv a pair representing a potential matched between and `A` and a `B`.
    * @return `true` if the matching is blocked by the given pair; `false` otherwise.
    * @note A `Matching` is blocked by a pair `(A, B)` if they each prefer one another to the partner that they
    *       have been matched with.
    */
  def isBlockedBy(kv: (A, B)): Boolean = {
    get(kv._2).exists(kv._2.ordering.gt(kv._1, _)) && get(kv._1).exists(kv._1.ordering.gt(kv._2, _))
  }

  def isEmpty: Boolean = {
    matches.isEmpty
  }

  def nonEmpty: Boolean = {
    matches.nonEmpty
  }

  def size: Int = {
    matches.size
  }

}
