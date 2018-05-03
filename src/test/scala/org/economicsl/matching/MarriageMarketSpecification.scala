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
package org.economicsl.matching

import org.scalacheck.{Gen, Properties}


object MarriageMarketSpecification extends Properties("marriage-market") {

  import org.scalacheck.Prop._

  val man: Gen[Man] = {
    for {
      id <- Gen.posNum[Long]
      quality <- Gen.posNum[Long]
    } yield Man(id, quality, Man.womanByQuality)
  }

  val woman: Gen[Woman] = {
    for {
      id <- Gen.posNum[Long]
      quality <- Gen.posNum[Long]
    } yield Woman(id, quality, Woman.manByQuality)
  }

  def setOfN[T](n: Int, g: Gen[T]): Gen[Set[T]] = {
    Gen.containerOfN[Set, T](n, g)
  }

  val unMatched: Gen[(List[Man], List[Woman])] = Gen.sized {
    size => for {
      ms <- Gen.containerOfN[List, Man](size, man)
      ws <- Gen.containerOfN[List, Woman](size, woman)
    } yield (ms, ws)
  }

  def isBlockedBy(matching: Matching[Woman, Man], pair: (Man, Woman)): Boolean = {
    matching.get(pair._2).exists(pair._2.ordering.gt(pair._1, _)) && matching.map(_.swap).get(pair._1).exists(pair._1.ordering.gt(pair._2, _))
  }

  val pairOfSetGen = Gen.sized {
    size => for {
      x <- Gen.containerOfN[Set, Man](size, man)
      y <- Gen.containerOfN[Set, Woman](size, woman)
    } yield (x,y)
  }

  property("test") = forAll(pairOfSetGen) { case (x,y) => println(x.size, y.size); true}

  /*property("all men and women are matched") = forAll(pairOfSetGen) {
    case (ms, ws) =>
      println((ms.size, ws.size))
      val (unMatchedMs, unMatchedWs, matches) = DeferredAcceptance.weaklyStableMatching(ms, ws)
      unMatchedMs.isEmpty && unMatchedWs.isEmpty && (matches.size == ms.size)
  }

  property("matching should be weakly stable") = forAll(pairOfSetGen) {
    case (ms, ws) =>
      val (_, _, matches) = DeferredAcceptance.weaklyStableMatching(ms.toSet, ws.toSet)
      ms.forall(m => ws.forall(w => !isBlockedBy(matches, (m, w))))
  }
*/
}
