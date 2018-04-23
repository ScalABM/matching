package org.economicsl.matching

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.parallel.immutable.{ParHashMap, ParHashSet}


object DeferredAcceptance {

  // define a couple of type aliases to simplify the API...UnMatched is a State monad!
  type UnMatched[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]] = (HashSet[A], HashSet[B])
  type Matched[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]] = HashMap[A, B]

  // define a couple of type aliases to simplify the API...ParUnMatched is a State monad!
  type ParUnMatched[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]] = (ParHashSet[A], ParHashSet[B])
  type ParMatched[A <: Proposer with Predicate[B] with Preferences[B], B <: Predicate[A] with Preferences[A]] = ParHashMap[A, B]

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
