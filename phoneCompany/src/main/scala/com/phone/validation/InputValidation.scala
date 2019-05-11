package com.phone.validation

import com.phone.model._
import cats.implicits._
import java.time.LocalTime
import scala.util.Try

object InputValidation {

  def validate(rawInputs : List[RawInput]) : Option[List[Call]] =
    rawInputs.traverse(
      (ri : RawInput) => for {
        pn <- PhoneNumber(ri.phoneNumber)
        time <- Try(LocalTime.parse(ri.duration)).toOption
      } yield Call(ri.costumerID, pn, time)
    )
}