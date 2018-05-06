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
package org.economicsl.matching.twosided.onetoone

import java.util.UUID

import org.economicsl.matching.{Preferences, Proposer}


case class Man(uuid: UUID, quality: Long, ordering: Ordering[Woman]) extends Proposer with Preferences[Woman]


object Man {

  implicit val womanByQuality: Ordering[Woman] = Ordering.by(w => w.quality)

}