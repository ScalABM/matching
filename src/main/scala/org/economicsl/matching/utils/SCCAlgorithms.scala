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
package org.economicsl.matching.utils


object SCCAlgorithms {

  type StronglyConnectedComponent[A] = Set[A]

  def tarjan[A](graph: DirectedGraph[A]): Set[StronglyConnectedComponent[A]] = {

    // todo: make this function tail-recursive!
    def strongConnect(state: TarjanState, v: A): TarjanState = {
      state.visited(v) match {
        case None =>
          graph.get(v) match {
            case Some(successors) =>
              successors.foldLeft(state.visit(v))((s, w) => strongConnect(s, w).updateLowLink(v, w)).collectScc(v)
            case None =>  // vertex v is a sink!
              state
          }
        case Some(_) =>
          state
      }
    }

    case class Visited(index: Int, lowLink: Int)

    case class TarjanState(visited: Map[A, Option[Visited]],
                           next: Int,
                           stack: List[A],
                           stacked: Map[A, Boolean],
                           components: Set[StronglyConnectedComponent[A]]) {

      def collectScc(v: A): TarjanState = {

        @annotation.tailrec
        def pop(stack: List[A], nodes: Set[A]): (List[A], Set[A]) = stack match {
          case Nil => (stack, nodes)
          case h :: t =>
            if (h == v) (t, nodes + h) else pop(t, nodes + h)
        }

        // If v is a root node, pop the stack and generate an SCC
        visited(v).get match {
          case Visited(index, lowLink) if index == lowLink =>
            val (residualStack, nodes) = pop(stack, Set.empty)
            val stackedLessScc = nodes.foldLeft(stacked)((s, w) => s.updated(w, false))
            copy(stack = residualStack, stacked = stackedLessScc, components = components + nodes)
          case _ => this
        }
      }

      def visit(v: A): TarjanState = copy(
        visited = visited.updated(v, Some(Visited(next, next))),
        next = next + 1,
        stack = v :: stack,
        stacked = stacked.updated(v, true)
      )

      def updateLowLink(v: A, w: A): TarjanState = {
        (visited(v), visited(w)) match {
          case (Some(vv), Some(ww)) if stacked(w) && (ww.lowLink < vv.lowLink) =>
            copy(visited.updated(v, Some(vv.copy(lowLink = ww.lowLink))))
          case _ => this
        }
      }

    }

    object State {
      def initial: TarjanState = {
        TarjanState(Map.empty.withDefaultValue(None), 0, List.empty, Map.empty.withDefaultValue(false), Set.empty)
      }
    }

    val finalState = graph.vertices.foldLeft(State.initial)((state, v) => strongConnect(state, v))
    finalState.components

  }

}