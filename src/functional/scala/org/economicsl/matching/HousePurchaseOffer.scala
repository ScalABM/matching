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

import java.util.UUID

import org.economicsl.core.Price


case class HousePurchaseOffer(uuid: UUID, issuer: Long, price: Price)
  extends Proposer with Predicate[HouseListing] with Preferences[HouseListing] {

  /** Boolean function used to determine whether some `HouseListing` is acceptable.
    *
    * @return a boolean function that returns `true` if the `HouseListing` is acceptable and `false` otherwise.
    */
  val isAcceptable: (HouseListing) => Boolean = {
    listing => listing.price <= price
  }

  /** An `Ordering` defined over `HouseListing` instances. */
  val ordering: Ordering[HouseListing] = Ordering.by(listing => (-listing.price.value, listing.house.quality, listing.uuid))

}

