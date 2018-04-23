package org.economicsl.matching

import org.economicsl.core.Price


case class HousePurchaseOffer(id: Long, issuer: Long, price: Price)
  extends Proposer with Predicate[HouseListing] with Preferences[HouseListing] {

  /** Boolean function used to determine whether some `HouseListing` is acceptable.
    *
    * @return a boolean function that returns `true` if the `HouseListing` is acceptable and `false` otherwise.
    */
  val isAcceptable: (HouseListing) => Boolean = {
    listing => listing.price <= price
  }

  /** An `Ordering` defined over `HouseListing` instances. */
  val ordering: Ordering[HouseListing] = Ordering.by(listing => (-listing.price.value, listing.house.quality, listing.id))


}

