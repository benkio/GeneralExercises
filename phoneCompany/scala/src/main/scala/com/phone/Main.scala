package com.phone

import cats.effect._
import com.phone.calculator._
import com.phone.model._
import com.phone.parser._
import com.phone.validation._

import scala.io._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    output <- program(args.headOption)
    _ <- IO(println(output))
  } yield ExitCode.Success

  def program(filePath : Option[String]) : IO[String] = for {
    rawInput <- Parser.inputStream(filePath.getOrElse("/calls.log")).use((inputStream: BufferedSource) => Parser.readRawInput(inputStream))
    optCalls: Option[List[Call]] = InputValidation.validate(rawInput)
    calls <- optCalls.fold(IO.raiseError[List[Call]](new Exception("validation error")))(IO.pure(_))
    bills = PhoneReport(calls).map(FeeCalculatorWithPromotion.calculate(_))
    result <- IO(bills.map((b: Bill) => b.toString).mkString("\n"))
  } yield result
}
