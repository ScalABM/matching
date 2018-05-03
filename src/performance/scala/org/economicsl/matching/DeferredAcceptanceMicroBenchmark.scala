package org.economicsl.matching

import java.util.UUID

import org.economicsl.core.Price
import org.scalameter.{Bench, Gen}

import scala.collection.immutable.HashSet
import scala.util.Random


object DeferredAcceptanceMicroBenchmark extends Bench.OnlineRegressionReport {

  val sizes: Gen[Int] = Gen.exponential("Number of buyers/sellers.")(factor=2, until=math.pow(2, 11).toInt, from=2)

  def randomHouse(): House = {
    House(UUID.randomUUID(), Random.nextLong())
  }

  def randomHouseListing(): HouseListing = {
    HouseListing(UUID.randomUUID(), Random.nextLong(), Price(Random.nextLong()), randomHouse())
  }

  def randomHousePurchaseOffer(): HousePurchaseOffer = {
    HousePurchaseOffer(UUID.randomUUID(), Random.nextLong(), Price(Random.nextLong()))
  }

  def randomHousePurchaseOffers(size: Int): HashSet[HousePurchaseOffer] = {
    @annotation.tailrec
    def accummulate(n: Int, offers: HashSet[HousePurchaseOffer]): HashSet[HousePurchaseOffer] = {
      if (n <= 0) {
        offers
      } else {
        accummulate(n - 1, offers + randomHousePurchaseOffer())
      }
    }
    accummulate(size, HashSet.empty[HousePurchaseOffer])
  }

  def randomHouseListings(size: Int): HashSet[HouseListing] = {
    @annotation.tailrec
    def accummulate(n: Int, listings: HashSet[HouseListing]): HashSet[HouseListing] = {
      if (n <= 0) {
        listings
      } else {
        accummulate(n - 1, listings + randomHouseListing())
      }
    }
    accummulate(size, HashSet.empty[HouseListing])
  }

  val unMatchedParticipants: Gen[(HashSet[HousePurchaseOffer], HashSet[HouseListing])] = for {
    size <- sizes
  } yield {
    val housePurchaseOffers: HashSet[HousePurchaseOffer] = randomHousePurchaseOffers(size / 2)
    val houseListings: HashSet[HouseListing] = randomHouseListings(size / 2)
    (housePurchaseOffers, houseListings)
  }

  performance of "DeferredAcceptance" in {
//    measure method "stableMatching" in {
//      using(unMatchedParticipants) in { case (buyers, sellers) =>
//        DeferredAcceptance.stableMatching(buyers, sellers)
//      }
//    }

    measure method "weaklyStableMatching" in {
      using(unMatchedParticipants) in { case (buyers, sellers) =>
        (new StableMarriageAlgorithm[HousePurchaseOffer, HouseListing]())(buyers, sellers)
      }
    }

  }

}
