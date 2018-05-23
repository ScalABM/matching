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
package org.economicsl.matching.onesided

import org.economicsl.matching.{MatchingTestSpecification, School, Student}
import org.scalacheck.{Gen, Prop, Properties}


object TopTradingCyclesAlgorithmSpecification extends Properties("ttc-specification") with MatchingTestSpecification {

  val allocations: Gen[Allocation[Student, School]] = Gen.sized {
    size => Gen.mapOfN(size, for { x <- student; y <- school } yield (x, y))
  }

  property("if agents have identical preferences, then final allocation should be the initial allocation") = Prop.forAll(allocations) { initial =>
    val result = TopTradingCyclesAlgorithm(initial)
    result.equals(initial)
  }


}