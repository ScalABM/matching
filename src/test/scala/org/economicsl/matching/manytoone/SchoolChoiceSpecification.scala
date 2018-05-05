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
package org.economicsl.matching.manytoone

import org.scalacheck.{Gen, Properties}


object SchoolChoiceSpecification extends Properties("school-choice") {

  import org.scalacheck.Prop._

  val randomStudent: Gen[Student] = {
    for {
      id <- Gen.uuid
      gpa <- Gen.chooseNum(1.0, 4.0)
    } yield Student(id, gpa, Student.anySchoolIsAcceptable, Student.schoolByQuality)
  }

  val randomSchool: Gen[School] = {
    for {
      id <- Gen.uuid
      quality <- Gen.chooseNum(0.0, 100.0)
      max <- Gen.size
      quota <- Gen.choose(0, max)
      minimum <- Gen.chooseNum(1.0, 4.0)
    } yield School(id, quality, quota, School.anyStudentIsAcceptable, School.studentByGPA)
  }

  val unMatched: Gen[(Set[Student], Set[School])] = Gen.sized {
    size => for {
      students <- Gen.containerOfN[Set, Student](size, randomStudent)
      schools <- Gen.nonEmptyContainerOf[Set, School](randomSchool)
    } yield (students, schools)
  }

  property("no school accepts more students than its quota allows") = forAll(unMatched) {
    case (students, schools) =>
      val ((_, _), matching) = (new DeferredAcceptanceAlgorithm[Student, School])(students, schools)
      matching.matches.forall{ case (school, matchedStudents) => matchedStudents.size <= school.quota }
  }

  property("matching should be stable") = forAll(unMatched) {
    case (students, schools) =>
      val ((_, _), matching) = (new DeferredAcceptanceAlgorithm[Student, School])(students, schools)
      students.forall(student => schools.forall(school => !matching.isBlockedBy(school -> student)))
  }

}

