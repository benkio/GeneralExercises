package com.phone.model

/**
  * Model a valid phone Number
  */
case class PhoneNumber private (number : String)

object PhoneNumber {
  def apply(number : String) : Option[PhoneNumber] =
    Some(number)
      .filter(_.matches("[0-9]{3}-[0-9]{3}-[0-9]{3}"))
      .map(new PhoneNumber(_))
}