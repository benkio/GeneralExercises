package com.phone.model

import org.scalacheck._
import org.scalacheck.Prop._
import com.phone.inputgenerator._

class PhoneNumberSpec extends Properties("PhoneNumber") {

  property("Apply on invalid numbers") = forAll { (invalidNumber : String) => PhoneNumber(invalidNumber) == None }
  property("Apply on valid numbers") = forAll(InputGenerator.validNumbers) { (validNumber : String) => {
                                                               val result = PhoneNumber(validNumber)
                                                               result.isDefined && (result.get.number == validNumber)
                                                             } }

}