package com.phone.model

import java.time.LocalTime
import org.scalacheck._
import org.scalacheck.Prop._
import com.phone.inputgenerator._

class CallSpec extends Properties("Call") {

  property("isWithinStandardRate with stardard rate duration (<= 3 min)") = forAll(InputGenerator.standardRateDuration) { (duration : LocalTime) => Call.isWithinStandardRate(duration) == true}

  property("isWithinStandardRate with overflow rate duration (> 3 min)") = forAll(InputGenerator.overflowDuration) { (duration : LocalTime) => Call.isWithinStandardRate(duration) == false}

  property("Apply with stardard rate duration") =
    forAll(InputGenerator.standardRateDuration,
           InputGenerator.phoneNumber) {
      (duration : LocalTime, ph : PhoneNumber) =>
      Call(InputGenerator.costumerIDTest, ph, duration) match {
        case _ : StandardRateCall => true
        case _ => false
      }
    }
  property("Apply with stardard rate duration") =
    forAll(InputGenerator.overflowDuration,
           InputGenerator.phoneNumber) {
      (duration : LocalTime, ph : PhoneNumber) =>
      Call(InputGenerator.costumerIDTest, ph, duration) match {
        case _ : OverflowRateCall => true
        case _ => false
      }
    }

}