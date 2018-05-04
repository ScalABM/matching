package org.economicsl.matching.manytoone

import java.util.UUID

import org.economicsl.matching.{Predicate, Preferences, Proposer}


case class Student(uuid: UUID, gpa: Double, isAcceptable: (School) => Boolean, ordering: Ordering[School])
  extends Proposer with Predicate[School] with Preferences[School]


object Student {

  implicit val schoolByQuality: Ordering[School] = Ordering.by(school => school.quality)

  val anySchoolIsAcceptable: (School) => Boolean = {
    _ => true
  }

}