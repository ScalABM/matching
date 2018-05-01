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

import org.scalatest.FlatSpec


class WeaklyStableMatchingSpec extends FlatSpec {

  "A weakly stable matching of two empty sets" should "be an empty map" in {
    val ((_, _), matching) = (new GaleShapleyAlgorithm[Man, Woman])(Set.empty[Man], Set.empty[Woman])
    assert(matching.isEmpty)
  }

  it should "throw IllegalArgumentException when attempting to match sets of different size." in {
    assertThrows[IllegalArgumentException] {
      (new GaleShapleyAlgorithm[Man, Woman])(Set(Man(1, 42, Man.womanByQuality)), Set.empty[Woman])
    }

    assertThrows[IllegalArgumentException] {
      (new GaleShapleyAlgorithm[Man, Woman])(Set.empty[Man], Set(Woman(1, 42, Woman.manByQuality)))
    }
  }

}
