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

import org.economicsl.matching.{Predicate, Preferences, Proposer}

import scala.collection.immutable.TreeSet


/** Class implementing the deferred acceptance algorithm for the "many-to-one" case.
  *
  * @tparam M the type of proposer.
  * @tparam W the type of proposee.
  */
class DeferredAcceptanceAlgorithm[M <: Proposer with Predicate[W] with Preferences[W],
                                  W <: Predicate[M] with Preferences[M] with Quota]
  extends ((Set[M], Set[W]) => ((Set[M], Set[W]), ManyToOneMatching[W, Set[M]])) {

    /** Compute a stable matching between two sets.
      *
      * @param ms set of proposers to be matched.
      * @param ws set of proposees to be matched.
      * @return
      * @note A matching will be called "stable" unless there is a pair each of which strictly prefers the other
      *       to its partner in the matching. This algorithm produces a weakly stable matching in `O(n^2)` time where `n`
      *       is the size of the inputs sets.
      */
    def apply(ms: Set[M], ws: Set[W]): ((Set[M], Set[W]), ManyToOneMatching[W, M]) = {

      @annotation.tailrec
      def accumulate(unMatchedMs: Set[M], toBeMatchedMs: Set[M], matches: Map[W, TreeSet[M]], rejected: Map[M, Set[W]]): (Set[M], Set[W], Map[W, Set[M]]) = {
        toBeMatchedMs.headOption match {
          case Some(toBeMatchedM) =>
            val previouslyRejected = rejected.getOrElse(toBeMatchedM, Set.empty)
            val acceptableWs = ws.diff(previouslyRejected)
            if (acceptableWs.isEmpty) {
              accumulate(unMatchedMs + toBeMatchedM, toBeMatchedMs - toBeMatchedM, matches, rejected)
            } else {
              val mostPreferredW = acceptableWs.max(toBeMatchedM.ordering)
              matches.get(mostPreferredW) match {
                case Some(matchedMs) if mostPreferredW.isAcceptable(toBeMatchedM) =>
                  if (matchedMs.size < mostPreferredW.quota) { // mostPreferredW has spots to fill!
                    val updatedToBeMatchedMs = toBeMatchedMs - toBeMatchedM
                    val updatedMatches = matches.updated(mostPreferredW, matchedMs + toBeMatchedM)
                    accumulate(unMatchedMs, updatedToBeMatchedMs, updatedMatches, rejected)
                  } else if (mostPreferredW.ordering.lt(matchedMs.head, toBeMatchedM)) {  // mostPreferredW has reached its quota!
                    val updatedToBeMatchedMs = toBeMatchedMs - toBeMatchedM + matchedMs.head
                    val updatedMatches = matches.updated(mostPreferredW, matchedMs - matchedMs.head + toBeMatchedM)
                    val updatedRejected = rejected.updated(matchedMs.head, rejected.getOrElse(matchedMs.head, Set.empty) + mostPreferredW)
                    accumulate(unMatchedMs, updatedToBeMatchedMs, updatedMatches, updatedRejected)
                  } else {
                    val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                    accumulate(unMatchedMs, toBeMatchedMs, matches, updatedRejected)
                  }
                case Some(_) => // toBeMatchedM proposal is not acceptable to mostPreferredW
                  val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                  accumulate(unMatchedMs, toBeMatchedMs, matches, updatedRejected)
                case None if mostPreferredW.isAcceptable(toBeMatchedM) => // mostPreferredW has yet to receive an acceptable offer!
                  val updatedMatches = matches + (mostPreferredW -> TreeSet(toBeMatchedM))
                  accumulate(unMatchedMs, toBeMatchedMs - toBeMatchedM, updatedMatches, rejected)
                case None =>  // toBeMatchedM proposal is not acceptable to mostPreferredW!
                  val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                  accumulate(unMatchedMs, toBeMatchedMs, matches, updatedRejected)
              }
            }
          case None =>
            (unMatchedMs, matches.keySet.diff(ws), matches)
        }
      }
      val unacceptableWs = ms.foldLeft(Map.empty[M, Set[W]])((z, m) => z + (m -> ws.filter(m.isAcceptable)))
      val (unMatchedMs, unMatchedWs, matches) = accumulate(Set.empty, ms, Map.empty, unacceptableWs)
      ((unMatchedMs, unMatchedWs), ManyToOneMatching(matches))
    }

  }
