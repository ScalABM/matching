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


/** Class for modeling directed graphs.
  *
  * @param graph internal representation of the graph that maps each vertex to its successors.
  * @tparam A the vertex type.
  */
class DirectedGraph[A] private(graph: Map[A, Set[A]]) {

  lazy val isEmpty: Boolean = {
    graph.forall{ case (_, vs) => vs.isEmpty }
  }

  lazy val nonEmpty: Boolean = {
    !isEmpty
  }

  lazy val stronglyConnectedComponents: Set[Set[A]] = {
    SCCAlgorithms.tarjan(this)
  }

  lazy val stronglyConnectedSubGraphs: Set[DirectedGraph[A]] = {
    stronglyConnectedComponents.map(cc => subGraph(cc))
  }

  lazy val vertices: Set[A] = {
    graph.keySet
  }

  final def + (v: A): DirectedGraph[A] = {
    if (graph.contains(v)) {
      this
    } else {
      new DirectedGraph(graph + (v -> Set.empty))
    }
  }

  final def + (edge: Edge[A]): DirectedGraph[A] = {
    val withAdditionalEdge = graph.get(edge.from) match {
      case Some(successors) =>
        new DirectedGraph(graph.updated(edge.from, successors + edge.to))
      case None =>
        new DirectedGraph(graph + (edge.from -> Set(edge.to)))
    }
    withAdditionalEdge + edge.to
  }

  final def apply(v: A): Set[A] = {
    graph(v)
  }

  final def get(v: A): Option[Set[A]] = {
    graph.get(v)
  }

  final def subGraph(vertices: Set[A]): DirectedGraph[A] = {
    val subGraph = vertices.foldLeft(Map.empty[A, Set[A]]){ case (m, v) =>
      val successors = graph.getOrElse(v, Set.empty)
      m + (v -> successors.intersect(vertices))
    }
    new DirectedGraph(subGraph)
  }

  override def toString: String = {
    graph.toString()
  }

}


object DirectedGraph {

  def cycle(length: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case from :: to :: ns => loop(to :: ns, graph + Edge(from, to))
      case last :: Nil => graph + Edge(last, 0)
      case Nil => graph
    }
    loop((0 until length).toList, DirectedGraph.empty)
  }

  def empty[A]: DirectedGraph[A] = {
    new DirectedGraph(Map.empty)
  }

  def loop(v: Int): DirectedGraph[Int] = {
    DirectedGraph.empty[Int] + Edge(v, v)
  }

  def loops(size: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case v :: vs => loop(vs, graph + Edge(v, v))
      case Nil => graph
    }
    loop((0 until size).toList, DirectedGraph.empty)
  }

  def path(length: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case from :: to :: ns => loop(to :: ns, graph + Edge(from, to))
      case _ => graph
    }
    loop((0 until length).toList, DirectedGraph.empty)
  }

  def sink(size: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int],graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case v :: vs => loop(vs, graph + Edge(v, 0))
      case Nil => graph
    }
    loop((0 until size).toList, DirectedGraph.empty)
  }

  def source(size: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case v :: vs => loop(vs, graph + Edge(0, v))
      case Nil => graph
    }
    loop((0 until size).toList, DirectedGraph.empty)
  }

}
