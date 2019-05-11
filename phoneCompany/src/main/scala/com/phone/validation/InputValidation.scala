package com.phone.validation

import java.time.LocalTime

import cats.implicits._
import com.phone.model._

import scala.util.Try

object InputValidation {

  def validate(rawInputs: List[RawInput]): Option[List[Call]] =
    rawInputs.traverse(
      (ri: RawInput) => for {
        pn <- PhoneNumber(ri.phoneNumber)
        time <- Try(LocalTime.parse(ri.duration)).toOption
      } yield Call(ri.costumerID, pn, time)
    )
}