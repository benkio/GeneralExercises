package com.phone.validation

import com.phone.model._
import org.scalacheck._
import org.scalacheck.Prop._
import com.phone.inputgenerator._

class InputValidationSpec extends Properties("InputValidation") {

  property("validate") = forAll(InputGenerator.rawInputs) {
    (ri : List[RawInput]) => {
      val result = InputValidation.validate(ri)
      result.isDefined &&
        result.get.size == ri.size &&
        result.get.map(_.costumerID) == ri.map(_.costumerID) &&
        result.get.map(_.called.number) == ri.map(_.phoneNumber) &&
        result.get.map(_.duration.toString) == ri.map(_.duration)
    }

  }
}