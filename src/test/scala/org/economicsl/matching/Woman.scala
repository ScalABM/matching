package org.economicsl.matching


case class Woman(id: Long, quality: Long, ordering: Ordering[Man]) extends Preferences[Man]


object Woman {

  val manByQuality: Ordering[Man] = Ordering.by(m => m.quality)

}
