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
package org.economicsl.matching.onetoone


import cats.data.State
import org.scalacheck.{Gen, Prop, Properties}


object MarriageMarketSpecification extends Properties("marriage-market") {

  val man: Gen[Man] = {
    for {
      id <- Gen.uuid
      quality <- Gen.posNum[Long]
    } yield Man(id, quality, Man.womanByQuality)
  }

  val woman: Gen[Woman] = {
    for {
      id <- Gen.uuid
      quality <- Gen.posNum[Long]
    } yield Woman(id, quality, Woman.manByQuality)
  }

  val unMatched: Gen[(Set[Man], Set[Woman])] = Gen.sized {
    size => for {
      ms <- Gen.containerOfN[Set, Man](size, man)
      ws <- Gen.containerOfN[Set, Woman](size, woman)
    } yield (ms, ws)
  }

  property("all men and women are matched") = Prop.forAll(unMatched) { unmatched =>
      val result = State(new StableMarriageAlgorithm[Man, Woman]).run(unmatched)
      val ((unMatchedMs, unMatchedWs), matching) = result.value
      unMatchedMs.isEmpty && unMatchedWs.isEmpty && (matching.size == unmatched._1.size)
  }

  property("matching should be stable") = Prop.forAll(unMatched) {unmatched =>
    val result = State(new StableMarriageAlgorithm[Man, Woman]).run(unmatched)
    val ((_, _), matching) = result.value
    unmatched._1.forall(m => unmatched._2.forall(w => !matching.isBlockedBy(w -> m)))
  }

}
