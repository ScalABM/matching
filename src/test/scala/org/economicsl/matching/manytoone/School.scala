package org.economicsl.matching.manytoone

import java.util.UUID

import org.economicsl.matching.{Predicate, Preferences}


case class School(uuid: UUID, quality: Double, quota: Int, isAcceptable: (Student) => Boolean, ordering: Ordering[Student])
  extends Predicate[Student] with Preferences[Student] with Quota


object School {

  implicit val studentByGPA: Ordering[Student] = Ordering.by(student => student.gpa)

  val anyStudentIsAcceptable: (Student) => Boolean = {
    _ => true
  }

  def gpaGreaterThan(threshold: Double)(student: Student): Boolean = {
    student.gpa >= threshold
  }

}