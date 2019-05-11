package com.phone.model

import org.scalacheck._
import org.scalacheck.Prop._
import wolfendale.scalacheck.regexp.RegexpGen

class PhoneNumberSpec extends Properties("PhoneNumber") {

  val validNumbers : Gen[String] = RegexpGen.from("[0-9]{3}-[0-9]{3}-[0-9]{3}")

  property("Apply on invalid numbers") = forAll { (invalidNumber : String) => PhoneNumber(invalidNumber) == None }
  property("Apply on valid numbers") = forAll(validNumbers) { (validNumber : String) => {
                                                               val result = PhoneNumber(validNumber)
                                                               result.isDefined && (result.get.number == validNumber)
                                                             } }

}