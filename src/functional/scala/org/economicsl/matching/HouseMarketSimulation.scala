package org.economicsl.matching

import org.economicsl.core.Price
import org.scalacheck.Gen

import scala.collection.immutable.HashSet


object HouseMarketSimulation extends App {

  // define some generators for the input data
  val houseGen: Gen[House] = {
    for {
      id <- Gen.posNum[Long]
      quality <- Gen.choose(1, 100)
    } yield House(id, quality)
  }

  val priceGen: Gen[Price] = {
    for {
      value <- Gen.choose(100000, 1000000)
    } yield Price(value)
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
      id <- Gen.choose(0L, Long.MaxValue)
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
