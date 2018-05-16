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
package org.economicsl.matching.twosided.manytoone

import org.economicsl.matching.{MatchingTestSpecification, School, Student}
import org.scalacheck.{Prop, Properties}


object SchoolChoiceSpecification extends Properties("school-choice") with MatchingTestSpecification {

  property("no school accepts more students than its quota allows") = Prop.forAll(unmatchedStudentsAndSchools) {
    case (students, schools) =>
      val ((_, _), matching) = (new DeferredAcceptanceAlgorithm[Student, School])(students, schools)
      matching.matches.forall{ case (school, matchedStudents) => matchedStudents.size <= school.quota }
  }

  property("matching should be stable") = Prop.forAll(unmatchedStudentsAndSchools) {
    case (students, schools) =>
      val ((_, _), matching) = (new DeferredAcceptanceAlgorithm[Student, School])(students, schools)
      students.forall(student => schools.forall(school => !matching.isBlockedBy(school -> student)))
  }

}

