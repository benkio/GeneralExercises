package com.phone

import org.scalatest._
import scala.io._
import cats.effect._

class MainSpec extends WordSpec with Matchers {

  val testFilePath : String = "/mainTest.log"
  val result : String = s"""B Bill - daily totalAmount: GBP 13.05
C Bill - daily totalAmount: GBP 0.00
A Bill - daily totalAmount: GBP 12.50"""

  "main.program" should {
    "return the expected result when with the mainTest file" in {
      Main.program(Some(testFilePath)).unsafeRunSync shouldEqual result
    }
  }
}
