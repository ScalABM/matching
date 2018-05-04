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


/** Class used to represent a many-to-one matching.
  *
  * @param matches
  * @tparam A
  * @tparam B
  */
case class ManyToOneMatching[A <: Preferences[B] with Quota, B <: Proposer with Preferences[A]](matches: Map[A, Set[B]]) {

  def get(a: A): Option[Set[B]] = {
    matches.get(a)
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
