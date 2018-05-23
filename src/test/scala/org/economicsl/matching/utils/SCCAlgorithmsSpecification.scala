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

import org.scalacheck.{Gen, Prop, Properties}


object SCCAlgorithmsSpecification extends Properties("scc-specification") {

  val pathOfLengthN: Gen[DirectedGraph[Int]] = Gen.sized {
    size => DirectedGraph.path(size)
  }

  val loops: Gen[DirectedGraph[Int]] = Gen.sized {
    size => DirectedGraph.loops(size)
  }

  val cycleOfLengthN: Gen[DirectedGraph[Int]] = Gen.sized {
    size => DirectedGraph.cycle(size)
  }

  val sinkOfSizeN: Gen[DirectedGraph[Int]] = Gen.sized {
    size => DirectedGraph.sink(size)
  }

  val sourceOfSizeN: Gen[DirectedGraph[Int]] = Gen.sized {
    size => DirectedGraph.source(size)
  }

  property("a path of length N should have N strongly connected components") = Prop.forAll(pathOfLengthN) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    sccs.equals(graph.vertices.map(v => Set(v)))
  }

  property("every non-empty cycle should have a single strongly connected component") = Prop.forAll(cycleOfLengthN) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    (sccs.size <= 1) && sccs.headOption.forall(scc => scc.equals(graph.vertices))
  }

  property("a sink of size N should have N strongly connected components") = Prop.forAll(sinkOfSizeN) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    sccs.equals(graph.vertices.map(v => Set(v)))
  }

  property("a source of size N should have N strongly connected components") = Prop.forAll(sourceOfSizeN) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    sccs.equals(graph.vertices.map(v => Set(v)))
  }

  property("every self-loop is considered a strongly connected component") = Prop.forAll(loops) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    sccs.equals(graph.vertices.map(v => Set(v)))
  }

}
