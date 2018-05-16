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


class TopTradingCyclesAlgorithm[A <: Preferences[B], B]
    extends (Allocation[A, B] => Allocation[A, B]) {

  def apply(initial: Allocation[A, B]): Allocation[A, B] = {

    @annotation.tailrec
    def loop(allocation: Allocation[A, B], result: Allocation[A, B]): Allocation[A, B] = {
      // for each A identify the A that has been allocated its most preferred B
      val topAs = allocation.map { case (a, _) =>
        val (topA, _) = allocation.maxBy { case (_, b) => b }(a.ordering)
        (a, Set(topA))
      }

      val tradingCycles = findTradingCycles(topAs)
      val reallocation = reallocate(allocation, tradingCycles)
      val residual = allocation.filter { case (a, _) => tradingCycles.forall(cycle => !cycle.contains(a)) }
      if (residual.isEmpty) {
        reallocation
      } else {
        loop(residual, result.foldLeft(reallocation)(_ + _))
      }
    }

    loop(initial, Map.empty[A, B])
  }

  /* Private implementation details */
  private[this] type TradingCycle = Map[A, A]
  private[this] type PreferredTradingPartners = Map[A, Set[A]]

  /** Finds all of the trading cycles in the top-trading graph.
    *
    * @param partners
    * @return
    * @note finding all trading cycles in the top trading graph is equivalent to finding all strongly connected
    *       components of the top trading graph.
    */
  private[this] def findTradingCycles(partners: PreferredTradingPartners): List[TradingCycle] = {

    def toTradingCycle(conversion: Map[Int, A])(scc: Map[Int, Set[Int]]): TradingCycle = {
      scc.map{ case (n, ns) => (conversion(n), conversion(ns.head)) }
    }

    val hashCodes = partners.map{ case (a, _) => (a, a.hashCode()) }
    val graphAdjList = partners.map{ case (k, vs) => (hashCodes(k), vs.map(v => hashCodes(v))) }
    val sccs = SCCAlgorithms.tarjan(graphAdjList)
    val conversion = hashCodes.map(_.swap)
    sccs.map(scc => toTradingCycle(conversion)(scc))
  }

  /** Re-allocate an initial allocation based on a collection of top-trading cycles.
    *
    * @param initial the initial allocation.
    * @param cycles a top trading cycles defining sequences of trades.
    * @return a new allocation.
    */
  private[this] def reallocate(initial: Allocation[A, B], cycles: List[TradingCycle]): Allocation[A, B] = {

    @annotation.tailrec
    def loop(cycles: List[TradingCycle], result: Allocation[A, B]): Allocation[A, B] = cycles match {
      case Nil =>
        result
      case c :: cs =>
        val partialAllocation = c.map{ case(a0, a1) => (a0, initial(a1)) }
        loop(cs, partialAllocation.foldLeft(result)(_ + _))  // OK because keys are unique!
    }

    loop(cycles, Map.empty[A, B])
  }

}
