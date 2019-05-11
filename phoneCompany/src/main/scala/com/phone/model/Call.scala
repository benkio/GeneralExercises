package com.phone.model

import java.time.LocalTime

/**
  * Model the call performed by the costumer
  */
case class Call(costumerID: String, called : PhoneNumber, duration: LocalTime)