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

import java.util.UUID

import org.scalatest.FlatSpec

import scala.util.{Failure, Success}


class StableMarriageAlgorithmSpec extends FlatSpec {

  "The stable marriage algorithm applied to two empty sets" should "successfully return an empty matching." in {
    val unmatched = (Set.empty[Man], Set.empty[Woman])
    val result = (new StableMarriageAlgorithm[Man, Woman])(unmatched)
    result match {
      case ((_, _), Success(matching)) =>
        assert(matching.isEmpty)
      case ((_,_), Failure(_)) =>
        false
    }
  }

  "The stable matching algorithm applied to sets of different sizes" should "return a failure." in {
    val unmatched1 = (Set(Man(UUID.randomUUID(), 42, Man.womanByQuality)), Set.empty[Woman])
    val ((_, _), result1) = (new StableMarriageAlgorithm[Man, Woman])(unmatched1)
    assert(result1.isFailure)

    val unmatched2 = (Set.empty[Man], Set(Woman(UUID.randomUUID(), 42, Woman.manByQuality)))
    val ((_, _), result2) = (new StableMarriageAlgorithm[Man, Woman])(unmatched2)
    assert(result2.isFailure)
  }

}
