package com.phone.calculator

import com.phone.model._
import org.joda.money._
import java.time.LocalTime
import org.scalacheck._
import org.scalacheck.Prop._
import com.phone.inputgenerator._

class FeeCalculatorSpec extends Properties("FeeCalculator") {

  val calculator : FeeCalculator = new FeeCalculator {}

  property("isWithinStandardRate with stardard rate call (<= 3 min)") = forAll(InputGenerator.standardRateCall) { (call : Call) => calculator.isWithinStandardRate(call) == true}

  property("isWithinStandardRate with overflow rate call (> 3 min)") = forAll(InputGenerator.overflowRateCall) { (call : Call) => calculator.isWithinStandardRate(call) == false}

}