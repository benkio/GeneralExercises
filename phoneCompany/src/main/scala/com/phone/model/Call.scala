package com.phone.model

import java.time.LocalTime

/**
  * Model the call performed by the costumer
  */
sealed trait Call {
  def costumerID: String

  def called: PhoneNumber

  def duration: LocalTime
}

case class StandardRateCall private(costumerID: String, called: PhoneNumber, duration: LocalTime) extends Call

case class OverflowRateCall private(costumerID: String, called: PhoneNumber, duration: LocalTime) extends Call

object Call {
  val standardRateDuration: LocalTime = LocalTime.of(0, 3)

  def isWithinStandardRate(duration: LocalTime): Boolean =
    duration.isBefore(standardRateDuration) || duration.equals(standardRateDuration)

  def apply(costumerID: String,
            called: PhoneNumber,
            duration: LocalTime): Call =
    if (isWithinStandardRate(duration)) StandardRateCall(costumerID, called, duration)
    else OverflowRateCall(costumerID, called, duration)
}