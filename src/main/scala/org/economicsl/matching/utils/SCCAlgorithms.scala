package org.economicsl.matching.utils

object SCCAlgorithms {

  type GraphAdjList = Map[Int, Set[Int]]
  type SubGraphAdjList = Map[Int, Set[Int]]

  def tarjan(graph: GraphAdjList): List[SubGraphAdjList] = {

    // todo: make this function tail-recursive!
    def strongConnect(state: State, v: Int): State = {
      state.visited(v) match {
        case None =>
          val successors = graph(v)
          if (successors.nonEmpty) {
            successors.foldLeft (state.visit (v) ) ((s, w) => strongConnect (s, w).updateLowLink (v, w) ).collectScc(v)
          } else {
            state
          }
        case Some(_) =>
          state
      }
    }

    graph.keySet.foldLeft(State.initial)((state, v) => strongConnect(state, v)).results.map(
      nodes => extractSubGraph(graph, nodes))

  }

  private case class Visited(index: Int, lowLink: Int)

  private case class State(visited: Map[Int, Option[Visited]],
                           next: Int,
                           stack: List[Int],
                           stacked: Map[Int, Boolean],
                           results: List[Set[Int]]) {

    def collectScc(v: Int): State = {

      @annotation.tailrec
      def pop(r: Set[Int], s: List[Int]): (Set[Int], List[Int]) = {
        if (s.head == v) (r + s.head, s.tail) else pop(r + s.head, s.tail)
      }

      // If v is a root node, pop the stack and generate an SCC
      visited(v).get match {
        case Visited(index, lowLink) if index == lowLink =>
          val (scc, remainingStack) = pop(Set.empty, stack)
          val stackedLessScc = scc.foldLeft(stacked)((s, w) => s updated(w, false))
          copy(stack = remainingStack, stacked = stackedLessScc, results = scc :: results)
        case _ => this
      }
    }

    def visit(i: Int): State = copy(
      visited = visited updated(i, Some(Visited(next, next))),
      next = next + 1,
      stack = i :: stack,
      stacked = stacked updated(i, true)
    )

    def updateLowLink(v: Int, w: Int): State = (visited(v).get, visited(w).get) match {
      case (vv, ww) if stacked(w) && ww.lowLink < vv.lowLink => copy(visited = visited updated(v, Some(Visited(vv.index, ww.lowLink))))
      case _ => this
    }
  }

  private object State {
    def initial: State = {
      State(Map.empty.withDefaultValue(None), 0, List.empty, Map.empty.withDefaultValue(false), List.empty)
    }
  }


  private def extractSubGraph(original: GraphAdjList, nodes: Set[Int]): SubGraphAdjList = {
    nodes.map(i => (i, original(i) intersect nodes)).toMap
  }

}