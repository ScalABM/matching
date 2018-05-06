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
package org.economicsl.matching.onetoone

import org.economicsl.matching.{Preferences, Proposer}

import scala.util.{Failure, Success, Try}


/** Class implementing the Gale-Shapley "stable marriage" Algorithm.
  *
  * @tparam M the type of proposer.
  * @tparam W the type of proposee.
  * @note The algorithm guarantees that all `M` and `W` are matched (i.e., all type `W` agents are acceptable matches
  *       for a type `M` agent and all type `M` agents are acceptable matches for a type `W` agent) and that the
  *       resulting matching will be stable. Furthermore the resulting matching will be optimal for the proposing type
  *       `M` agents.
  *
  *       [[http://www.eecs.harvard.edu/cs286r/courses/fall09/papers/dubbinsfreedman.pdf ''Dubins and Freedman (1981)'']]
  *       and [[https://pdfs.semanticscholar.org/cf51/08ca274bdf762193ffdc8d2b2c95208af1b0.pdf ''Roth (1982)'']]
  *       proved that is a weakly dominant strategy for type `M` agents to submit truthful preferences over type `W`
  *       agents. Type `W` agents, on the other hand, may have incentives to mis-state their true preferences over type
  *       `M` agents.
  * @see For a non-mathematical description of the algorithm see
  *      [[http://www.eecs.harvard.edu/cs286r/courses/fall09/papers/galeshapley.pdf ''Gale and Shapley (1962)'']].
  */
class StableMarriageAlgorithm[M <: Proposer with Preferences[W], W <: Preferences[M]]
  extends ((Set[M], Set[W]) => Try[((Set[M], Set[W]), OneToOneMatching[W, M])]) {

  /** Compute a stable matching between two sets of equal size.
    *
    * @param unmatched
    * @return a stable matching between proposees (`ws`) and proposers (`ms`).
    */
  def apply(ms: Set[M], ws: Set[W]): Try[((Set[M], Set[W]), OneToOneMatching[W, M])] = {

    @annotation.tailrec
    def accumulate(unMatchedMs: Set[M], matches: Map[W, M], rejected: Map[M, Set[W]]): (Set[M], Set[W], Map[W, M]) = {
      unMatchedMs.headOption match {
        case Some(unMatchedM) =>
          val previouslyRejected = rejected.getOrElse(unMatchedM, Set.empty)
          val mostPreferredW = unmatched._2.diff(previouslyRejected).max(unMatchedM.ordering)
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
          (unMatchedMs, unmatched._2.diff(matches.keySet), matches)
      }
    }

    if (ms.size == ws.size) {
      val (unMatchedMs, unMatchedWs, matches) = accumulate(ms, Map.empty, Map.empty)
      Success(((unMatchedMs, unMatchedWs), OneToOneMatching(matches)))
    }
    else {
      Failure(new IllegalArgumentException(s"The size of ms is ${ms.size} which does not equal the size of ws which is ${ws.size}!"))
    }

  }

}
