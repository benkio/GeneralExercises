package com.phone.inputgenerator

import com.phone.model._
import java.time.LocalTime
import org.scalacheck._
import wolfendale.scalacheck.regexp.RegexpGen
import org.joda.money._

object InputGenerator {
  val validNumbers : Gen[String] = RegexpGen.from("[0-9]{3}-[0-9]{3}-[0-9]{3}")
  def localTime(secRange : Range = Range(0, 59),
                minRange : Range  = Range(0, 59),
                hourRange : Range = Range(0, 23)) : Gen[LocalTime] = for {
    sec <- Gen.choose(secRange.start, secRange.end)
    min <- Gen.choose(minRange.start, minRange.end)
    hours <- Gen.choose(hourRange.start, hourRange.end)
  } yield LocalTime.of(hours, min, sec)

  val standardRateDuration : Gen[LocalTime] =
    localTime(secRange = Range(0,59),
              minRange = Range(0, 2),
              hourRange = Range(0, 0))
      .map(_.plusSeconds(1))
  val overflowDuration : Gen[LocalTime] =
    localTime(secRange = Range(0,59),
              minRange = Range(4, 59),
              hourRange = Range(0, 23))

  val phoneNumber : Gen[PhoneNumber] = validNumbers.map((num : String) => PhoneNumber(num).get)
  def call(costumerGen : Option[Gen[String]] = None,
           duration : Option[Gen[LocalTime]] = None) : Gen[Call] = for {
    costumerID <- costumerGen.getOrElse(Arbitrary(Gen.nonEmptyListOf[Char](Arbitrary.arbChar.arbitrary).map(_.mkString)).arbitrary)
    ph <- phoneNumber
    lt <- duration.getOrElse(localTime())
  } yield Call(costumerID, ph, lt)

  val costumerIDTest : String = "Same Costumer"
  val standardRateCall : Gen[Call] = call(duration = Some(standardRateDuration))
  val overflowRateCall : Gen[Call] = call(duration = Some(overflowDuration))
  val sameCostumerCalls : Gen[List[Call]] = Gen.listOfN(5 ,call(Some(Gen.const(costumerIDTest))))
  val calls : Gen[List[Call]] = Gen.listOfN(5 ,call())
  val phoneReport : Gen[PhoneReport] = sameCostumerCalls.map(PhoneReport(_).head)

  def bill(costumerGen : Option[Gen[String]] = None,
           totalAmount : Option[Gen[Int]] = None) : Gen[Bill] = for {
    costumerID <- costumerGen.getOrElse(Arbitrary(Gen.nonEmptyListOf[Char](Arbitrary.arbChar.arbitrary).map(_.mkString)).arbitrary)
    total <- totalAmount.getOrElse(Gen.choose(0, Int.MaxValue))
  } yield Bill(costumerID, Money.of(CurrencyUnit.GBP, total))
}