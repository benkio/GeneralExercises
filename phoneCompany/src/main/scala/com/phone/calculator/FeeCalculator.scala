package com.phone.calculator

import com.phone.model._
import org.joda.money._

/**
  * Master trait containing the standard way to calculate a Bill
  */
trait FeeCalculator {
  val standardRatePerSecond: Money = Money.of(CurrencyUnit.GBP, 0.05)
  val overflowRatePerSecond: Money = Money.of(CurrencyUnit.GBP, 0.03)

  def calculate(call: Call): Money = call match {
    case StandardRateCall(_, _, d) => standardRatePerSecond.multipliedBy(d.toSecondOfDay)
    case OverflowRateCall(_, _, d) => standardRatePerSecond.multipliedBy(180).plus(
      overflowRatePerSecond.multipliedBy(d.minusMinutes(3).toSecondOfDay)
    )
  }

  def calculate(report: PhoneReport): Bill =
    Bill(report.costumerID,
      report.calls.map(calculate(_)).fold(Money.zero(CurrencyUnit.GBP))((m1: Money, m2: Money) => m1.plus(m2))
    )
}

object FeeCalculatorWithPromotion extends FeeCalculator {
  def applyPromotion(pr: PhoneReport): PhoneReport =
    PhoneReport(pr.costumerID,
      pr.calls.sortBy(_.duration).init)

  override def calculate(call: PhoneReport): Bill =
    super.calculate(applyPromotion(call))
}