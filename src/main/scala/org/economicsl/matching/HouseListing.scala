package org.economicsl.matching

import org.economicsl.core.Price


case class HouseListing(id: Long, issuer: Long, price: Price, house: House)
  extends Predicate[HousePurchaseOffer] with Preferences[HousePurchaseOffer] {

  /** Boolean function used to determine whether some `HousePurchaseOffer` is acceptable.
    *
    * @return a boolean function that returns `true` if the `HousePurchaseOffer` is acceptable and `false` otherwise.
    */
  val isAcceptable: (HousePurchaseOffer) => Boolean = {
    purchaseOffer => purchaseOffer.price >= price
  }

  /** An `Ordering` defined over `HousePurchaseOffer` instances. */
  val ordering: Ordering[HousePurchaseOffer] = Ordering.by(purchaseOffer => (purchaseOffer.price, purchaseOffer.id))

}
