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


class DirectedGraph[A] private(val vertices: Set[A], graph: Map[A, Set[A]]) {

  lazy val isEmpty: Boolean = {
    graph.forall{ case (_, vs) => vs.isEmpty }
  }

  lazy val nonEmpty: Boolean = {
    !isEmpty
  }

  def + (edge: (A, A)): DirectedGraph[A] = {
    val (from, to) = edge
    val successors = graph.getOrElse(from, Set.empty)
    new DirectedGraph(vertices + from + to, graph.updated(from, successors + to).updated(to, graph.getOrElse(to, Set.empty)))
  }

  def apply(v: A): Set[A] = {
    graph(v)
  }

  def get(v: A): Option[Set[A]] = {
    graph.get(v)
  }

  def subGraph(vertices: Set[A]): DirectedGraph[A] = {
    val subGraph = vertices.foldLeft(Map.empty[A, Set[A]]){ case (m, v) =>
      val successors = graph.getOrElse(v, Set.empty)
      m + (v -> successors.intersect(vertices))
    }
    new DirectedGraph(vertices, subGraph)
  }

  def stronglyConnectedComponents: Set[Set[A]] = {
    SCCAlgorithms.tarjan(this)
  }

  def stronglyConnectedSubGraphs: Set[DirectedGraph[A]] = {
    stronglyConnectedComponents.map(cc => subGraph(cc))
  }

  override def toString: String = {
    graph.toString()
  }

}


object DirectedGraph {

  def cycle(length: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case from :: to :: ns => loop(to :: ns, graph + (from -> to))
      case last :: Nil => graph + (last -> 0)
      case Nil => graph
    }
    loop((0 until length).toList, DirectedGraph.empty)
  }

  def empty[A]: DirectedGraph[A] = {
    new DirectedGraph(Set.empty, Map.empty)
  }

  def loop(v: Int): DirectedGraph[Int] = {
    DirectedGraph.empty + (v -> v)
  }

  def loops(size: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case v :: vs => loop(vs, graph + (v -> v))
      case Nil => graph
    }
    loop((0 until size).toList, DirectedGraph.empty)
  }

  def path(length: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case from :: to :: ns => loop(to :: ns, graph + (from -> to))
      case _ => graph
    }
    loop((0 until length).toList, DirectedGraph.empty)
  }

  def sink(size: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int],graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case v :: vs => loop(vs, graph + (v -> 0))
      case Nil => graph
    }
    loop((0 until size).toList, DirectedGraph.empty)
  }

  def source(size: Int): DirectedGraph[Int] = {
    @annotation.tailrec
    def loop(vertices: List[Int], graph: DirectedGraph[Int]): DirectedGraph[Int] = vertices match {
      case v :: vs => loop(vs, graph + (0 -> v))
      case Nil => graph
    }
    loop((0 until size).toList, DirectedGraph.empty)
  }

}
