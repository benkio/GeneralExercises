package com.phone.calculator

import com.phone.model._
import org.joda.money._
import java.time.LocalTime
import org.scalacheck._
import org.scalacheck.Prop._
import com.phone.inputgenerator._

class FeeCalculatorSpec extends Properties("FeeCalculator") {

  val calculator : FeeCalculator = new FeeCalculator {}

  property("calculate on standard calls") = forAll(InputGenerator.standardRateCall) {
    (call : Call) => calculator.calculate(call) == Money.of(CurrencyUnit.GBP, 0.05).multipliedBy(call.duration.toSecondOfDay)
  }

  property("calculate on overflow calls") = forAll(InputGenerator.overflowRateCall) {
    (call : Call) => calculator.calculate(call) == (Money.of(CurrencyUnit.GBP, 0.05).multipliedBy(180).plus(Money.of(CurrencyUnit.GBP, 0.03).multipliedBy(call.duration.minusMinutes(3).toSecondOfDay)))
  }

  property("calculate on phone report") = forAll(InputGenerator.phoneReport) {
    (phoneReport : PhoneReport) =>
    calculator.calculate(phoneReport) == Bill(phoneReport.costumerID,
                                              phoneReport.calls.map(calculator.calculate(_)).fold(Money.zero(CurrencyUnit.GBP))((m1 : Money, m2 : Money) => m1.plus(m2)))
  }

  // FeeCalculatorWithPromotion ///////////////////////////////////////////////

  property("applyPromotion") = forAll(InputGenerator.phoneReport) {
    (pr : PhoneReport) => {
      val maxCall = pr.calls.maxBy(_.duration)
      val result = FeeCalculatorWithPromotion.applyPromotion(pr).calls
      result.size == (pr.calls.size - 1) && pr.calls.diff(result).head == maxCall
    }
  }


  property("calculate with promotion") = forAll(InputGenerator.phoneReport) {
    (pr : PhoneReport) => {
      val maxCall = pr.calls.maxBy(_.duration)
      val result = FeeCalculatorWithPromotion.calculate(pr)
      result.totalAmount == (pr.calls.map(calculator.calculate(_)).fold(Money.zero(CurrencyUnit.GBP))((m1 : Money, m2 : Money) => m1.plus(m2))).minus(calculator.calculate(maxCall))
    }
  }
}