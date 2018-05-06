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
  extends (((Set[M], Set[W])) => ((Set[M], Set[W]), ManyToOneMatching[W, M])) {

    /** Compute a stable matching between two sets.
      *
      * @param unmatched
      * @return
      * @note A matching will be called "stable" unless there is a pair each of which strictly prefers the other
      *       to its partner in the matching. This algorithm produces a weakly stable matching in `O(n^2)` time where `n`
      *       is the size of the inputs sets.
      */
    def apply(unmatched: (Set[M], Set[W])): ((Set[M], Set[W]), ManyToOneMatching[W, M]) = {
      val (ms, ws) = unmatched

      @annotation.tailrec
      def accumulate(unMatched: Set[M], toBeMatched: Set[M], matches: Map[W, TreeSet[M]], rejected: Map[M, Set[W]]): (Set[M], Set[W], Map[W, TreeSet[M]]) = {
        toBeMatched.headOption match {
          case Some(toBeMatchedM) =>
            val previouslyRejected = rejected.getOrElse(toBeMatchedM, Set.empty)
            val acceptableWs = ws.diff(previouslyRejected)
            if (acceptableWs.isEmpty) {
              accumulate(unMatched + toBeMatchedM, toBeMatched - toBeMatchedM, matches, rejected)
            } else {
              val mostPreferredW = acceptableWs.max(toBeMatchedM.ordering)
              matches.get(mostPreferredW) match {
                case Some(matchedMs) if mostPreferredW.isAcceptable(toBeMatchedM) =>
                  matchedMs.headOption match {
                    case Some(_) if matchedMs.size < mostPreferredW.quota =>
                      val updatedToBeMatchedMs = toBeMatched - toBeMatchedM
                      val updatedMatches = matches.updated(mostPreferredW, matchedMs + toBeMatchedM)
                      accumulate(unMatched, updatedToBeMatchedMs, updatedMatches, rejected)
                    case Some(leastPreferredMatchedM) if mostPreferredW.ordering.lt(leastPreferredMatchedM, toBeMatchedM) =>
                      val updatedToBeMatchedMs = toBeMatched - toBeMatchedM + leastPreferredMatchedM
                      val updatedMatches = matches.updated(mostPreferredW, matchedMs - leastPreferredMatchedM + toBeMatchedM)
                      val updatedRejected = rejected.updated(leastPreferredMatchedM, rejected.getOrElse(leastPreferredMatchedM, Set.empty) + mostPreferredW)
                      accumulate(unMatched, updatedToBeMatchedMs, updatedMatches, updatedRejected)
                    case Some(leastPreferredMatchedM) if mostPreferredW.ordering.gteq(leastPreferredMatchedM, toBeMatchedM) =>
                      val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                      accumulate(unMatched, toBeMatched, matches, updatedRejected)
                    case None =>
                      val updatedToBeMatchedMs = toBeMatched - toBeMatchedM
                      val updatedMatches = matches.updated(mostPreferredW, matchedMs + toBeMatchedM)
                      accumulate(unMatched, updatedToBeMatchedMs, updatedMatches, rejected)
                  }
                case Some(_) if mostPreferredW.isNotAcceptable(toBeMatchedM) =>
                  val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                  accumulate(unMatched, toBeMatched, matches, updatedRejected)
                case None if mostPreferredW.isAcceptable(toBeMatchedM) =>
                  val updatedMatches = matches + (mostPreferredW -> TreeSet(toBeMatchedM)(mostPreferredW.ordering))
                  accumulate(unMatched, toBeMatched - toBeMatchedM, updatedMatches, rejected)
                case None if mostPreferredW.isNotAcceptable(toBeMatchedM) =>
                  val updatedRejected = rejected.updated(toBeMatchedM, previouslyRejected + mostPreferredW)
                  accumulate(unMatched, toBeMatched, matches, updatedRejected)
              }
            }
          case None =>
            (unMatched, matches.keySet.diff(ws), matches)
        }
      }

      val unacceptableWs = ms.foldLeft(Map.empty[M, Set[W]])((z, m) => z + (m -> ws.filter(m.isAcceptable)))
      val (unMatchedMs, unMatchedWs, matches) = accumulate(Set.empty, ms, Map.empty, unacceptableWs)
      ((unMatchedMs, unMatchedWs), ManyToOneMatching(matches))
    }

  }
