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
package org.economicsl.matching.manytoone

import org.economicsl.matching.{Preferences, Proposer}

import scala.collection.immutable.TreeSet


/** Class used to represent a many-to-one matching.
  *
  * @param matches
  * @tparam A
  * @tparam B
  */
case class ManyToOneMatching[A <: Preferences[B] with Quota, B <: Proposer with Preferences[A]](matches: Map[A, TreeSet[B]]) {

  lazy val invertedMatches: Map[B, A] = matches.flatMap{ case (a, bs) => bs.map(b => (b, a)) }

  def get(a: A): Option[Set[B]] = {
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
    val (a, b) = kv
    get(b).exists(b.ordering.gt(a, _)) && get(a).exists(bs => a.ordering.gt(b, bs.head))
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
