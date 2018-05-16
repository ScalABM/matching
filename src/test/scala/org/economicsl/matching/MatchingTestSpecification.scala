package org.economicsl.matching

import org.scalacheck.Gen


trait MatchingTestSpecification {

  val man: Gen[Man] = {
    for {
      id <- Gen.uuid
      quality <- Gen.posNum[Long]
    } yield Man(id, quality, Man.womanByQuality)
  }

  val woman: Gen[Woman] = {
    for {
      id <- Gen.uuid
      quality <- Gen.posNum[Long]
    } yield Woman(id, quality, Woman.manByQuality)
  }

  val student: Gen[Student] = {
    for {
      id <- Gen.uuid
      gpa <- Gen.chooseNum(1.0, 4.0)
    } yield Student(id, gpa, Student.anySchoolIsAcceptable, Student.schoolByQuality)
  }

  val school: Gen[School] = {
    for {
      id <- Gen.uuid
      quality <- Gen.chooseNum(0.0, 100.0)
      max <- Gen.size
      quota <- Gen.choose(0, max)
      minimum <- Gen.chooseNum(1.0, 4.0)
    } yield School(id, quality, quota, School.anyStudentIsAcceptable, School.studentByGPA)
  }

  val unmatchedStudentsAndSchools: Gen[(Set[Student], Set[School])] = Gen.sized {
    size => for {
      students <- Gen.containerOfN[Set, Student](size, student)
      schools <- Gen.nonEmptyContainerOf[Set, School](school)
    } yield (students, schools)
  }


  val unmatchedMenAndWomen: Gen[(Set[Man], Set[Woman])] = Gen.sized {
    size => for {
      nm <- Gen.choose(0, size)
      ms <- Gen.containerOfN[Set, Man](nm, man)
      nw <- Gen.choose(0, size)
      ws <- Gen.containerOfN[Set, Woman](nw, woman)
    } yield (ms, ws)
  }


}
