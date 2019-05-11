package com.phone.model

import org.scalacheck._
import org.scalacheck.Prop._
import wolfendale.scalacheck.regexp.RegexpGen
import com.phone.model._
import com.phone.inputgenerator._

class PhoneReportSpec extends Properties("PhoneReport") {

  property("Apply on one costumer calls") = forAll(InputGenerator.sameCostumerCalls) {
    (calls : List[Call]) => {
      val result : List[PhoneReport] = PhoneReport(calls)
      result.size == 1 && result.head.costumerID == InputGenerator.costumerIDTest
    }
  }

  property("Apply on generic calls") = forAll(InputGenerator.calls) {
    (calls : List[Call]) => {
      val result : List[PhoneReport] = PhoneReport(calls)
      val costumers : Int = calls.map(_.costumerID).distinct.size
      result.size == costumers
    }
  }

}