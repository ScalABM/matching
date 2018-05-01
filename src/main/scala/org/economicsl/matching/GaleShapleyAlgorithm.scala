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

import org.scalactic.TripleEquals._


/** Class implementing the Gale-Shapley Algorithm.
  *
  * @tparam M the type of proposer.
  * @tparam W the type of proposee.
  * @note The algorithm guarantees that all `M` and `W` are matched and that the matching will be stable.
  */
class GaleShapleyAlgorithm[M <: Proposer with Preferences[W], W <: Preferences[M]]
  extends ((Set[M], Set[W]) => ((Set[M], Set[W]), Matching[W, M])) {

  /** Compute a weakly stable matching between two sets of equal size.
    *
    * @param ms set of proposers to be matched.
    * @param ws set of proposees to be matched.

    * @return a stable matching between proposees (`ws`) and proposers (`ms`).
    */
  def apply(ms: Set[M], ws: Set[W]): ((Set[M], Set[W]), Matching[W, M]) = {
    require(ms.size === ws.size)

    @annotation.tailrec
    def accumulate(unMatchedMs: Set[M], matches: Map[W, M], rejected: Map[M, Set[W]]): (Set[M], Set[W], Map[W, M]) = {
      unMatchedMs.headOption match {
        case Some(unMatchedM) =>
          val previouslyRejected = rejected.getOrElse(unMatchedM, Set.empty)
          val mostPreferredW = ws.diff(previouslyRejected).max(unMatchedM.ordering)
          matches.get(mostPreferredW) match {
            case Some(m) if mostPreferredW.ordering.lt(m, unMatchedM) => // mostPreferredW has received strictly better offer!
              val updatedUnMatchedMs = unMatchedMs - unMatchedM + m
              val updatedMatched = matches.updated(mostPreferredW, unMatchedM)
              val updatedRejected = rejected.updated(m, rejected.getOrElse(m, Set.empty) + mostPreferredW)
              accumulate(updatedUnMatchedMs, updatedMatched, updatedRejected)
            case Some(m) if mostPreferredW.ordering.gteq(m, unMatchedM) =>  // mostPreferredW already has weakly better offer!
              val updatedRejected = previouslyRejected + mostPreferredW
              accumulate(unMatchedMs, matches, rejected.updated(unMatchedM, updatedRejected))
            case None => // mostPreferredW has yet to receive any offer!
              accumulate(unMatchedMs - unMatchedM, matches + (mostPreferredW -> unMatchedM), rejected)
          }
        case None =>
          assert(unMatchedMs.isEmpty)
          (unMatchedMs, ws.diff(matches.keySet), matches)
      }
    }
    val (unMatchedMs, unMatchedWs, matches) = accumulate(ms, Map.empty, Map.empty)
    ((unMatchedMs, unMatchedWs), Matching(matches))

  }

}
