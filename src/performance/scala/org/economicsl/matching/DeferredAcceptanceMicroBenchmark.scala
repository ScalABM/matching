package org.economicsl.matching

import org.economicsl.core.Price
import org.scalameter.{Bench, Gen}

import scala.collection.immutable.HashSet
import scala.util.Random


object DeferredAcceptanceMicroBenchmark extends Bench.OnlineRegressionReport {

  def randomHouse(): House = {
    House(Random.nextLong(), Random.nextLong())
  }

  def randomHouseListing(): HouseListing = {
    HouseListing(Random.nextLong(), Random.nextLong(), Price(Random.nextLong()), randomHouse())
  }

  def randomHousePurchaseOffer(): HousePurchaseOffer = {
    HousePurchaseOffer(Random.nextLong(), Random.nextLong(), Price(Random.nextLong()))
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


  performance of "DeferredAcceptance" in {

    val sizes = Gen.exponential("Number of buyers/sellers.")(factor=2, until=math.pow(2, 11).toInt, from=2)

    val unMatchedParticipants = for {
      size <- sizes
    } yield {
      val housePurchaseOffers = randomHousePurchaseOffers(size / 2)
      val houseListings = randomHouseListings(size / 2)
      (housePurchaseOffers, houseListings)
    }

    val parUnMatchedParticipants = for {
      size <- sizes
    } yield {
      val housePurchaseOffers = randomHousePurchaseOffers(size / 2)
      val houseListings = randomHouseListings(size / 2)
      (housePurchaseOffers.par, houseListings.par)
    }

    measure method "stableMatching" in {
      using(unMatchedParticipants) in { case (buyers, sellers) =>
        DeferredAcceptance.stableMatching(buyers, sellers)
      }
    }

    measure method "parStableMatching" in {
      using(parUnMatchedParticipants) in { case (buyers, sellers) =>
        DeferredAcceptance.parStableMatching(buyers, sellers)
      }
    }
  }

}
