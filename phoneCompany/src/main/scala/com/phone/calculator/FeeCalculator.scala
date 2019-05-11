package com.phone.calculator

import com.phone.model._
import org.joda.money._
import java.time.LocalTime

/**
  * Master trait containing the standard way to calculate a Bill
  */
trait FeeCalculator {

  val standardRateDuration : LocalTime = LocalTime.of(0, 3)
  val standardRatePerSecond : Money = Money.of(CurrencyUnit.GBP, 0.05)
  val overflowRatePerSecond : Money = Money.of(CurrencyUnit.GBP, 0.03)

  def isWithinStandardRate(call : Call) : Boolean =
    call.duration.isBefore(standardRateDuration) || call.duration.equals(standardRateDuration)

  def calculate(call : Call) : Money = ???
  def calculate(call : PhoneReport) : Bill = ???
}

object FeeCalculatorWithPromotion extends FeeCalculator {
  def applyPromotion(bill : Bill) : Bill = ???

  override def calculate(call : PhoneReport) : Bill = ???
}