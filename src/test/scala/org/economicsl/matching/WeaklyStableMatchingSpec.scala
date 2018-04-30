package org.economicsl.matching

import org.scalatest.FlatSpec


class WeaklyStableMatchingSpec extends FlatSpec {

  "A weakly stable matching of two empty sets" should "be an empty map" in {
    val (_, _, matching) = DeferredAcceptance.weaklyStableMatching(Set.empty[Man], Set.empty[Woman])
    assert(matching.isEmpty)
  }

  it should "throw IllegalArgumentException when attempting to match sets of different size." in {
    assertThrows[IllegalArgumentException] {
      DeferredAcceptance.weaklyStableMatching(Set(Man(1, 42, Man.womanByQuality)), Set.empty[Woman])
    }

    assertThrows[IllegalArgumentException] {
      DeferredAcceptance.weaklyStableMatching(Set.empty[Man], Set(Woman(1, 42, Woman.manByQuality)))
    }
  }

}
