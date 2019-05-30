package com.phone.parser

import com.phone.model.RawInput
import org.scalatest._
import scala.io._
import cats.effect._

class ParserSpec extends WordSpec with Matchers {

  val testFilePath : String = "/test.log"
  val testFile : BufferedSource = Source.fromURL(getClass.getResource(testFilePath))
  val testContent : String = s"""A 555-333-212 00:02:03
A 555-433-242 00:06:41
A 555-433-242 00:01:03
B 555-333-212 00:01:20
A 555-333-212 00:01:10
A 555-663-111 00:02:09
A 555-333-212 00:04:28
B 555-334-789 00:00:03
A 555-663-111 00:02:03
B 555-334-789 00:00:53
B 555-971-219 00:09:51
B 555-333-212 00:02:03
B 555-333-212 00:04:31
B 555-334-789 00:01:59

"""

  "inputStream" should {
    "return the bufferedSource containing the expected content" in {
      Parser.inputStream(testFilePath).use {
        case x : BufferedSource => IO(x.mkString)
      }.unsafeRunSync shouldEqual testContent
    }
  }

  "parseRawInput" should {
    "return the expected RawInput" in {
      Parser.parseRawInput(testContent.lines.toList.head).unsafeRunSync shouldEqual RawInput("A", "555-333-212", "00:02:03")
    }
  }

  "readRawInput" should {
    "return the expected list of inputs" in {
      val result = Parser.readRawInput(testFile).unsafeRunSync
      result.size shouldEqual 14
      result shouldEqual List(
        RawInput("A", "555-333-212", "00:02:03"),
        RawInput("A", "555-433-242", "00:06:41"),
        RawInput("A", "555-433-242", "00:01:03"),
        RawInput("B", "555-333-212", "00:01:20"),
        RawInput("A", "555-333-212", "00:01:10"),
        RawInput("A", "555-663-111", "00:02:09"),
        RawInput("A", "555-333-212", "00:04:28"),
        RawInput("B", "555-334-789", "00:00:03"),
        RawInput("A", "555-663-111", "00:02:03"),
        RawInput("B", "555-334-789", "00:00:53"),
        RawInput("B", "555-971-219", "00:09:51"),
        RawInput("B", "555-333-212", "00:02:03"),
        RawInput("B", "555-333-212", "00:04:31"),
        RawInput("B", "555-334-789", "00:01:59")
      )
    }
  }

}
