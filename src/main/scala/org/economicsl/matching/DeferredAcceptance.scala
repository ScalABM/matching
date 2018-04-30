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
import scala.collection.immutable.{HashMap, HashSet}


object DeferredAcceptance {

  // define a couple of type aliases to simplify the API...UnMatched is a State monad!
  type UnMatched[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]] = (HashSet[A], HashSet[B])
  type Matched[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]] = HashMap[A, B]

  /** Stable Matching via Gale-Shapley Deferred-Acceptance algorithm.
    *
    * @param as
    * @param bs
    * @tparam A
    * @tparam B
    * @return a tuple whose first element is a tuple containing the set of unmatched `A` instances and the set of
    *         unmatched `B` instances, the second element of the tuple is a stable matching between `A` and `B` instances.
    */
  def stableMatching[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]]
                    (as: HashSet[A], bs: HashSet[B])
                    : (UnMatched[A, B], Matched[A, B]) = {

    @annotation.tailrec
    def accummulate(unMatchedAs: HashSet[A], unMatchedBs: HashSet[B], matches: HashMap[A, B]): (UnMatched[A, B], Matched[A, B]) = {
      if (unMatchedAs.isEmpty || unMatchedBs.isEmpty) {
        ((unMatchedAs, unMatchedBs), matches)
      } else {
        val additionalMatches = unMatchedAs.aggregate(HashMap.empty[B, HashSet[A]])(
          { case (potentialMatches, a) =>
            val acceptableBs = unMatchedBs.filter(a.isAcceptable)
            if (acceptableBs.isEmpty) {  // N.B. an acceptable B may not exist!
              potentialMatches
            } else {  // N.B. same B could be most preferred for multiple As!
              val mostPreferredB = acceptableBs.max(a.ordering)
              val potentialAs = potentialMatches.getOrElse(mostPreferredB, HashSet.empty[A])
              potentialMatches.updated(mostPreferredB, potentialAs + a)
            }
          }, { case (potentialMatches, morePotentialMatches) =>
            potentialMatches.merged(morePotentialMatches)({ case ((k, v1), (_, v2)) => (k, v1 ++ v2) })
          }
        ).aggregate(HashMap.empty[A, B])(
          { case (finalizedMatches, (b, potentialAs)) =>
            val acceptableAs = potentialAs.filter(b.isAcceptable)
            if (acceptableAs.isEmpty) {  // N.B. an acceptable A may not exist!
              finalizedMatches
            } else {
              finalizedMatches + (acceptableAs.max(b.ordering) -> b)
            }
          }, { case (finalizedMatches, moreFinalizedMatches) =>
            finalizedMatches ++ moreFinalizedMatches
          }
        )

        if (additionalMatches.isEmpty) {
          ((unMatchedAs, unMatchedBs), matches)
        } else {
          val remainingUnMatchedAs = unMatchedAs.diff(additionalMatches.keySet)
          val remainingUnMatchedBs = unMatchedBs.diff(additionalMatches.values.toSet)
          accummulate(remainingUnMatchedAs, remainingUnMatchedBs, matches ++ additionalMatches)
        }
      }
    }
    accummulate(as, bs, HashMap.empty[A, B])

  }

  /** Compute a weakly stable matching between two sets of equal size.
    *
    * @param ms set of proposers to be matched.
    * @param ws set of proposees to be matched.
    * @tparam M the type of proposer.
    * @tparam W the type of proposee.
    * @return a weakly stable matching between proposees (`ws`) and proposers (`ms`).
    * @note A matching will be called "weakly stable" unless there is a pair each of which strictly prefers the other
    *       to its partner in the matching. This algorithm produces a weakly stable matching in `O(n^2)` time where `n`
    *       is the size of the inputs sets.
    *
    *       From Dubins and Freedman (1981) and Roth (1982) it is a weakly dominant strategy for a type `M` agent to
    *       state its true preferences.
    */
  def weaklyStableMatching[M <: Proposer with Preferences[W], W <: Preferences[M]](ms: Set[M], ws: Set[W]): (Set[M], Set[W], Map[W, M]) = {
    require(ms.size === ws.size)

    @annotation.tailrec
    def accumulate(unMatchedMs: Set[M], matched: Map[W, M], rejected: Map[M, Set[W]]): (Set[M], Set[W], Map[W, M]) = {
      unMatchedMs.headOption match {
        case Some(unMatchedM) =>
          val previouslyRejected = rejected.getOrElse(unMatchedM, Set.empty)
          val mostPreferredW = ws.diff(previouslyRejected).max(unMatchedM.ordering)
          matched.get(mostPreferredW) match {
            case Some(m) if mostPreferredW.ordering.lt(m, unMatchedM) => // mostPreferredW has received strictly better offer!
              val updatedUnMatchedMs = unMatchedMs - unMatchedM + m
              val updatedMatched = matched.updated(mostPreferredW, unMatchedM)
              val updatedRejected = rejected.updated(m, rejected.getOrElse(m, Set.empty) + mostPreferredW)
              accumulate(updatedUnMatchedMs, updatedMatched, updatedRejected)
            case Some(m) if mostPreferredW.ordering.gteq(m, unMatchedM) =>  // mostPreferredW already has weakly better offer!
              val updatedRejected = previouslyRejected + mostPreferredW
              accumulate(unMatchedMs, matched, rejected.updated(unMatchedM, updatedRejected))
            case None => // mostPreferredW has yet to receive any offer!
              accumulate(unMatchedMs - unMatchedM, matched + (mostPreferredW -> unMatchedM), rejected)
          }
        case None =>
          assert(unMatchedMs.isEmpty)
          (unMatchedMs, ws.diff(matched.keySet), matched)
      }
    }
    accumulate(ms, Map.empty, Map.empty)
  }

  /** Compute a weakly stable matching between two sets.
    *
    * @param ms set of proposers to be matched.
    * @param ws set of proposees to be matched.
    * @tparam M the type of proposer.
    * @tparam W the type of proposee.
    * @return
    * @note A matching will be called "weakly stable" unless there is a pair each of which strictly prefers the other
    *       to its partner in the matching. This algorithm produces a weakly stable matching in `O(n^2)` time where `n`
    *       is the size of the inputs sets.
    */
  def weaklyStableMatching2[M <: Proposer with Predicate[W] with Preferences[W],
                            W <: Predicate[M] with Preferences[M]]
                           (ms: Set[M], ws: Set[W])
                           : (Set[M], Set[W], Map[W, M]) = {

    @annotation.tailrec
    def accumulate(unMatchedMs: Set[M], toBeMatchedMs: Set[M], matched: Map[W, M], rejected: Map[M, Set[W]]): (Set[M], Set[W], Map[W, M]) = {
      toBeMatchedMs.headOption match {
        case Some(toBeMatchedM) =>
          val previouslyRejected = rejected.getOrElse(toBeMatchedM, Set.empty)
          val acceptableWs = ws.diff(previouslyRejected)
          if (acceptableWs.isEmpty) {
            accumulate(unMatchedMs + toBeMatchedM, toBeMatchedMs - toBeMatchedM, matched, rejected)
          } else {
            val mostPreferredW = acceptableWs.max(toBeMatchedM.ordering)
            matched.get(mostPreferredW) match {
              case Some(m) if mostPreferredW.ordering.lt(m, toBeMatchedM) => // mostPreferredW has received strictly better offer!
                val updatedToBeMatchedMs = toBeMatchedMs - toBeMatchedM + m
                val updatedMatched = matched.updated(mostPreferredW, toBeMatchedM)
                val updatedRejected = rejected.updated(m, rejected.getOrElse(m, Set.empty) + mostPreferredW)
                accumulate(unMatchedMs, updatedToBeMatchedMs, updatedMatched, updatedRejected)
              case Some(m) if mostPreferredW.ordering.gteq(m, toBeMatchedM) => // mostPreferredW already has weakly better offer!
                val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                accumulate(unMatchedMs, toBeMatchedMs, matched, updatedRejected)
              case None if mostPreferredW.isAcceptable(toBeMatchedM) => // mostPreferredW has yet to receive an acceptable offer!
                accumulate(unMatchedMs, toBeMatchedMs - toBeMatchedM, matched + (mostPreferredW -> toBeMatchedM), rejected)
              case None =>  // unMatchedM proposal is not acceptable to mostPreferredW!
                val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                accumulate(unMatchedMs, toBeMatchedMs, matched, updatedRejected)
            }
          }
        case None =>
          (unMatchedMs, matched.keySet.diff(ws), matched)
      }
    }
    val unacceptableWs = ms.foldLeft(Map.empty[M, Set[W]])((z, m) => z + (m -> ws.filter(m.isAcceptable)))
    accumulate(Set.empty, ms, Map.empty, unacceptableWs)
  }

}
