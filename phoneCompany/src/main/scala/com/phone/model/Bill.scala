package com.phone.model

import org.joda.money._

/**
  * The output of the program 
  */
case class Bill private(costumerID : String, totalAmount : Money)
