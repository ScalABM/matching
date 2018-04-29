package org.economicsl.matching


case class Man(id: Long, quality: Long, ordering: Ordering[Woman]) extends Proposer with Preferences[Woman]


object Man {

  implicit val womanByQuality: Ordering[Woman] = Ordering.by(w => w.quality)

}