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


class DirectedGraph(adjacencyLists: Map[Int, Set[Int]]) {

  def get(v: Int): Option[Set[Int]] = {
    adjacencyLists.get(v)
  }

  def vertices: Set[Int] = {
    adjacencyLists.keySet
  }

  def subGraph(vertices: Set[Int]): DirectedGraph = {
    val subGraphAdjacencyLists = vertices.foldLeft(Map.empty[Int, Set[Int]]){ case (subGraph, v) =>
      val successors = adjacencyLists.getOrElse(v, Set.empty)
      subGraph + (v -> successors.intersect(vertices))
    }
    new DirectedGraph(subGraphAdjacencyLists)
  }

}


object DirectedGraph {

  def cycle(length: Int): DirectedGraph = {
    @annotation.tailrec
    def loop(vertices: List[Int], adjacencyLists: Map[Int, Set[Int]]): Map[Int, Set[Int]] = vertices match {
      case from :: to :: ns => loop(to :: ns, adjacencyLists + (from -> Set(to)))
      case last :: Nil =>
        adjacencyLists + (last -> Set(0))
      case Nil => adjacencyLists
    }
    new DirectedGraph(loop((0 until length).toList, Map.empty))
  }

  def empty: DirectedGraph = {
    new DirectedGraph(Map.empty)
  }

  def empty(size: Int): DirectedGraph = {
    @annotation.tailrec
    def loop(vertices: List[Int], adjacencyLists: Map[Int, Set[Int]]): Map[Int, Set[Int]] = vertices match {
      case v :: vs =>
        loop(vs, adjacencyLists + (v -> Set.empty))
      case _ =>
        adjacencyLists
    }
    new DirectedGraph(loop((0 until size).toList, Map.empty))
  }

  def loop(v: Int): DirectedGraph = {
    new DirectedGraph(Map(v -> Set(v)))
  }

  def loops(size: Int): DirectedGraph = {
    @annotation.tailrec
    def loop(vertices: List[Int], adjacencyLists: Map[Int, Set[Int]]): Map[Int, Set[Int]] = vertices match {
      case v :: vs =>
        loop(vs, adjacencyLists + (v -> Set(v)))
      case Nil =>
        adjacencyLists
    }
    new DirectedGraph(loop((0 until size).toList, Map.empty))
  }

  def path(length: Int): DirectedGraph = {
    @annotation.tailrec
    def loop(vertices: List[Int], adjacencyLists: Map[Int, Set[Int]]): Map[Int, Set[Int]] = vertices match {
      case from :: to :: ns =>
        loop(to :: ns, adjacencyLists + (from -> Set(to)))
      case _ =>
        adjacencyLists
    }
    new DirectedGraph(loop((0 until length).toList, Map.empty))
  }

  def sink(size: Int): DirectedGraph = {
    @annotation.tailrec
    def loop(vertices: List[Int], adjacencyLists: Map[Int, Set[Int]]): Map[Int, Set[Int]] = vertices match {
      case v :: vs =>
        loop(vs, adjacencyLists + (v -> Set(0)))
      case Nil => adjacencyLists
    }
    new DirectedGraph(loop((0 until size).toList, Map.empty))
  }

  def source(size: Int): DirectedGraph = {
    @annotation.tailrec
    def loop(vertices: List[Int], adjacencyLists: Map[Int, Set[Int]]): Map[Int, Set[Int]] = vertices match {
      case v :: vs =>
        loop(vs, adjacencyLists + (0 -> Set(v)))
      case Nil => adjacencyLists
    }
    new DirectedGraph(loop((0 until size).toList, Map.empty))
  }

}
