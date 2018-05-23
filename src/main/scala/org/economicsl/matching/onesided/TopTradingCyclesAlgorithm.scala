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
package org.economicsl.matching.onesided

import org.economicsl.matching.Preferences
import org.economicsl.matching.utils.{DirectedGraph, Edge}


/** Implements the top-trading cycles (TTC) algorithm for trading indivisible items without using "money."
  *
  * @note the resulting allocation is core stable meaning that no subset (or coalition) of agents can find a feasible
  *       allocation that makes members of the coalition better off.  The TTC algorithm is also "truthful" in that it
  *       is a weakly dominant strategy for all agents to truthfully report their preferences.
  */
object TopTradingCyclesAlgorithm {

  def apply[A <: Preferences[B], B](initial: Allocation[A, B]): Allocation[A, B] = {

    @annotation.tailrec
    def loop(current: Allocation[A, B], result: Allocation[A, B]): Allocation[A, B] = {
      val preferredTradingPartners = identifyTradingPartners(current)
      assert(preferredTradingPartners.nonEmpty)
      val (tradingCycles, residualTradingPartners) = partitionTradingPartners(preferredTradingPartners)
      val reallocation = reallocate(tradingCycles, current)
      if (residualTradingPartners.isEmpty) {
        reallocation.foldLeft(result)(_ + _)
      } else {
        val residualAllocation = current.filter{ case (a, _) => residualTradingPartners.contains(a) }
        loop(residualAllocation, reallocation.foldLeft(result)(_ + _))
      }
    }

    if (initial.isEmpty) initial else loop(initial, Map.empty)
  }

  /* Private implementation details */
  private[this] type TradingCycles[A <: Preferences[_]] = Set[DirectedGraph[A]]
  private[this] type PreferredTradingPartners[A <: Preferences[_]] = DirectedGraph[A]

  private[this] def identifyTradingPartners[A <: Preferences[B], B](allocation: Allocation[A, B]) = {
    @annotation.tailrec
    def loop(agents: Set[A], graph: PreferredTradingPartners[A]): PreferredTradingPartners[A] = {
      agents.headOption match {
        case None =>
          graph
        case Some(a) =>
          val (preferredTradingPartner, _) = allocation.maxBy { case (_, b) => b }(a.ordering)
          loop(agents - a, graph + Edge(a, preferredTradingPartner))
      }
    }
    loop(allocation.keySet, DirectedGraph.empty[A])
  }

  /** Partitions the graph of preferred trading partners into a collection of disjoint trading cycles and a collection
    * of agents that are not participating in any trading cycle.
    *
    * @param graph a graph mapping each agent to its most preferred trading partner.
    * @return
    * @note finding all trading cycles in the preferred trading partners graph is equivalent to finding all of the
    *       graph's strongly connected components.
    */
  private[this] def partitionTradingPartners[A <: Preferences[_]](graph: PreferredTradingPartners[A]) = {
    val (tradingCycles, emptySubGraphs) = graph.stronglyConnectedSubGraphs.partition(subGraph => subGraph.nonEmpty)
    val residualTradingPartners = emptySubGraphs.flatMap(subGraph => subGraph.vertices)
    (tradingCycles, residualTradingPartners)
  }

  /** Use a collection of trading cycles to re-allocate an initial allocation.
    *
    * @param tradingCycles a top trading cycles defining sequences of trades.
    * @param initial the initial allocation.
    * @return a new allocation.
    */
  private[this] def reallocate[A <: Preferences[B], B](tradingCycles: TradingCycles[A], initial: Allocation[A, B]) = {
    tradingCycles.aggregate(Map.empty[A, B])(
      { case (allocation, g) => g.vertices.foldLeft(allocation){ case (m, a) => m + (a -> initial(g(a).head)) } },
      { case (m1, m2) => m2.foldLeft(m1)(_ + _) }  // OK because keys are unique!
    )
  }

}
