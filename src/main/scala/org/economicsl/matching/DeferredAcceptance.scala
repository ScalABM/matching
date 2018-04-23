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

}
