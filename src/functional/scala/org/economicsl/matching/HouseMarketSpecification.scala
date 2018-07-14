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

import cats.data.State
import org.economicsl.core.Price
import org.economicsl.matching.onetoone.DeferredAcceptanceAlgorithm
import org.scalacheck.{Gen, Prop, Properties}


object HouseMarketSpecification extends Properties("housing-market") {

  // this generator should exist in esl-core!
  def price: Gen[Price] = {
    for {
      value <- Gen.posNum[Long]
    } yield Price(value)
  }

  val house: Gen[House] = {
    for {
      uuid <- Gen.uuid
      quality <- Gen.posNum[Int]
    } yield House(uuid, quality)
  }

  val houseListing: Gen[HouseListing] = {
    for {
      uuid <- Gen.uuid
      issuer <-  Gen.uuid
      price <-  price
      house <- house
    } yield HouseListing(uuid, issuer, price, house)
  }

  val housePurchaseOffer: Gen[HousePurchaseOffer] = {
    for {
      uuid <- Gen.uuid
      issuer <- Gen.uuid
      price <- price
    } yield HousePurchaseOffer(uuid, issuer, price)
  }

  def unMatched: Gen[(Set[HousePurchaseOffer], Set[HouseListing])] = Gen.sized {
    size => for {
      offers <- Gen.containerOfN[Set, HousePurchaseOffer](size, housePurchaseOffer)
      listing <- Gen.containerOfN[Set, HouseListing](size, houseListing)
    } yield (offers, listing)

  }

  property("matching should be incentive-compatible") = Prop.forAll(unMatched) { unmatched =>
        val result = State(new DeferredAcceptanceAlgorithm[HousePurchaseOffer, HouseListing]).run(unmatched)
        val ((_, _), matching) = result.value
        matching.matches.forall{ case (offer, listing) => offer.price >= listing.price }
  }

  property("matching should be stable") =  Prop.forAll(unMatched) { unmatched =>
    val result = State(new DeferredAcceptanceAlgorithm[HousePurchaseOffer, HouseListing]).run(unmatched)
    val ((_, _), matching) = result.value
    val (offers, listings) = unmatched
    offers.forall(o => listings.forall(l => !matching.isBlockedBy(l -> o)))
  }

}
