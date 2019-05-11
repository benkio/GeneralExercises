package com.phone.inputgenerator

import com.phone.model._
import java.time.LocalTime
import org.scalacheck._
import wolfendale.scalacheck.regexp.RegexpGen

object InputGenerator {
  val validNumbers : Gen[String] = RegexpGen.from("[0-9]{3}-[0-9]{3}-[0-9]{3}")
  def call(costumerGen : Option[Gen[String]]) : Gen[Call] = for {
    costumerID <- costumerGen.fold(Arbitrary(Gen.nonEmptyListOf[Char](Arbitrary.arbChar.arbitrary).map(_.mkString)).arbitrary)(identity)
    called <- validNumbers
  sec <- Gen.choose(0, 59)
  min <- Gen.choose(0, 59)
  hours <- Gen.choose(0, 23)
  } yield Call(costumerID, PhoneNumber(called).get, LocalTime.of(hours, min, sec))

  val costumerIDTest : String = "Same Costumer"
  val sameCostumerCalls : Gen[List[Call]] = Gen.listOfN(5 ,call(Some(Gen.const(costumerIDTest))))
  val calls : Gen[List[Call]] = Gen.listOfN(5 ,call(None))
}