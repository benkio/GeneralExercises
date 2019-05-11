package com.phone.model

/**
  * Intermediate type grouping a specific costumer with its calls.
  * No Money calculations are performed 
  */
case class PhoneReport private(costumerID : String, calls : List[Call])

object PhoneReport {
  def apply(calls : List[Call]) : List[PhoneReport] = ???
}