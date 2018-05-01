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

import org.economicsl.core.Price
import org.scalacheck.{Gen, Properties}

import scala.collection.immutable.HashSet


object HouseMarketSpecification extends Properties("housing-market") {

  import org.scalacheck.Prop._

  // this generator should exist in esl-core!
  val priceGen: Gen[Price] = {
    for {
      value <- Gen.choose(100000, 1000000)
    } yield Price(value)
  }

  val houseGen: Gen[House] = {
    for {
      id <- Gen.posNum[Long]
      quality <- Gen.choose(1, 100)
    } yield House(id, quality)
  }

  val houseListingGen: Gen[HouseListing] = {
    for {
      id <- Gen.posNum[Long]
      issuer <-  Gen.posNum[Long]
      price <-  priceGen
      house <- houseGen
    } yield HouseListing(id, issuer, price, house)
  }

  val housePurchaseOfferGen: Gen[HousePurchaseOffer] = {
    for {
      id <- Gen.posNum[Long]
      issuer <- Gen.posNum[Long]
      price <- priceGen
    } yield HousePurchaseOffer(id, issuer, price)
  }

  val housePurchaseOffers: Gen[HashSet[HousePurchaseOffer]] = Gen.sized {
    n => Gen.containerOfN[HashSet, HousePurchaseOffer](n, housePurchaseOfferGen)
  }

  val houseListings: Gen[HashSet[HouseListing]] = Gen.sized {
    n => Gen.containerOfN[HashSet, HouseListing](n, houseListingGen)
  }

  property("incentive-compatibility") = forAll(housePurchaseOffers, houseListings) {
    case (offers, listings) =>
      val ((_, _), matching) = (new DeferredAcceptanceAlgorithm[HousePurchaseOffer, HouseListing])(offers, listings)
      matching.matches.forall{ case (offer, listing) => offer.price >= listing.price }
  }

  property("matching should stable") =  forAll(housePurchaseOffers, houseListings) {
    case (offers, listings) =>
      val ((_, _), matching) = (new DeferredAcceptanceAlgorithm[HousePurchaseOffer, HouseListing])(offers, listings)
      offers.forall(o => listings.forall(l => !matching.isBlockedBy(l -> o)))
  }

}
