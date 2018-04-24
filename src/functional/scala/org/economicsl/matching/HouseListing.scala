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
