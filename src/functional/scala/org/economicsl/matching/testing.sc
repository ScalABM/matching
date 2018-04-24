import org.economicsl.core.Price
import org.economicsl.matching.{DeferredAcceptance, House, HouseListing, HousePurchaseOffer}

import scala.collection.immutable.HashSet


val purchaseOffers = HashSet(
  HousePurchaseOffer(1, 1, Price(100)),
  HousePurchaseOffer(2, 2, Price(105)),
  HousePurchaseOffer(3, 4, Price(105))
)

val listings = HashSet(
  HouseListing(1, 3, Price(102), House(1, 15)),
  HouseListing(2, 5, Price(100), House(2, 5)),
  HouseListing(3, 6, Price(190), House(3, 100))
)


val ((_, _), matches) = DeferredAcceptance.stableMatching(purchaseOffers, listings)