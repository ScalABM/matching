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

  val singleton: Gen[(Int, Set[Int])] = {
    for {
      n <- Gen.posNum[Int]
    } yield (n, Set.empty)
  }

  val loop: Gen[(Int, Set[Int])] = {
    for {
      n <- Gen.posNum[Int]
    } yield (n, Set(n))
  }

  val emptyGraph: Gen[Map[Int, Set[Int]]] = Gen.sized {
    size => Gen.mapOfN(size, singleton)
  }

  val graphWithOnlyLoops: Gen[Map[Int, Set[Int]]] = Gen.sized {
    size => Gen.mapOfN(size, loop)
  }

  val cycleOfLengthN: Gen[Map[Int, Set[Int]]] = Gen.sized {
    @annotation.tailrec
    def loop(c: List[Int], m: Map[Int, Set[Int]]): Map[Int, Set[Int]] = c match {
      case from :: to :: ns => loop(to :: ns, m + (from -> Set(to)))
      case last :: Nil =>
        m + (last -> Set(1))
      case Nil => m
    }

    size => loop((1 to size).toList, Map.empty)
  }

  property("an empty graph should have zero strongly connected components") = Prop.forAll(emptyGraph) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    sccs.isEmpty
  }

  property("every self-loop is considered a strongly connected component") = Prop.forAll(graphWithOnlyLoops) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    graph.size == sccs.size
  }

  property("every non-empty cycle should have a single strongly connected component") = Prop.forAll(cycleOfLengthN) { graph =>
    val sccs = SCCAlgorithms.tarjan(graph)
    sccs.isEmpty || (sccs.size == 1)
  }


}
