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

  property("all men and women are matched") = forAll(unMatched) {
    case (ms, ws) =>
      val ((unMatchedMs, unMatchedWs), matching) = (new GaleShapleyAlgorithm[Man, Woman])(ms, ws)
      unMatchedMs.isEmpty && unMatchedWs.isEmpty && (matching.size == ms.size)
  }

  property("matching should be stable") = forAll(unMatched) {
    case (ms, ws) =>
      val ((_, _), matching) = (new GaleShapleyAlgorithm[Man, Woman])(ms, ws)
      ms.forall(m => ws.forall(w => !matching.isBlockedBy(w -> m)))
  }

}
