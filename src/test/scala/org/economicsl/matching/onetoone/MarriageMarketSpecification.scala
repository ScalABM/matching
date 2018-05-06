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

import scala.util.{Failure, Success}


object MarriageMarketSpecification extends Properties("marriage-market") {

  val randomMan: Gen[Man] = {
    for {
      id <- Gen.uuid
      quality <- Gen.posNum[Long]
    } yield Man(id, quality, Man.womanByQuality)
  }

  val randomWoman: Gen[Woman] = {
    for {
      id <- Gen.uuid
      quality <- Gen.posNum[Long]
    } yield Woman(id, quality, Woman.manByQuality)
  }

  val randomUnmatchedSets: Gen[(Set[Man], Set[Woman])] = Gen.sized {
    size => for {
      nm <- Gen.choose(0, size)
      ms <- Gen.containerOfN[Set, Man](nm, randomMan)
      nw <- Gen.choose(0, size)
      ws <- Gen.containerOfN[Set, Woman](nw, randomWoman)
    } yield (ms, ws)
  }

  property("all men and women are matched") = Prop.forAll(randomUnmatchedSets) {
    case unmatched @ (ms, ws) =>
      val result = State(new StableMarriageAlgorithm[Man, Woman]).run(unmatched)
      val ((unMatchedMs, unMatchedWs), matching) = result.value
      matching match {
        case Success(oneToOneMatching) =>
          (ms.size == ws.size) && unMatchedMs.isEmpty && unMatchedWs.isEmpty && (oneToOneMatching.size == ms.size)
        case Failure(_) =>
          ms.size != ws.size
      }
  }

  property("matching should be stable") = Prop.forAll(randomUnmatchedSets) {
    case unmatched @ (ms, ws) =>
      val result = State(new StableMarriageAlgorithm[Man, Woman]).run(unmatched)
      val ((_, _), matching) = result.value
      matching match {
        case Success(oneToOneMatching) =>
          ms.forall(m => ws.forall(w => !oneToOneMatching.isBlockedBy(w -> m)))
        case Failure(_) =>
          ms.size != ws.size
      }
  }

}
