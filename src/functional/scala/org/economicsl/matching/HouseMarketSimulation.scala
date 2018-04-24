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
import org.scalacheck.Gen

import scala.collection.immutable.HashSet


object HouseMarketSimulation extends App {

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

  val housePurchaseOffers = Gen.containerOfN[HashSet, HousePurchaseOffer](1000, housePurchaseOfferGen)

  val houseListings = Gen.containerOfN[HashSet, HouseListing](1000, houseListingGen)

  (housePurchaseOffers.sample, houseListings.sample) match {
    case (Some(purchaseOffers), Some(listings)) =>
      val ((unMatchedPurchaseOffers, unMatchedListings), matches) = DeferredAcceptance.stableMatching(purchaseOffers, listings)
      println(s"Number of unmatched home buyers: ${unMatchedPurchaseOffers.size}")
      println(s"Number of unmatched home sellers: ${unMatchedListings.size}")
      println(s"Number of matched buyers and sellers: ${matches.size}")
    case _ => ???
  }

}
