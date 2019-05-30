package com.analytics.constants

import java.time.Duration
import scala.concurrent.duration._

/**
  * TimeConstants
  * Group all the vals about timing
  *
  */
object TimeConstants {
  val validVisitWindow : Duration = Duration.ofHours(1)
  val aggregatedTimeWindow : FiniteDuration = FiniteDuration(10, SECONDS)
}