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
