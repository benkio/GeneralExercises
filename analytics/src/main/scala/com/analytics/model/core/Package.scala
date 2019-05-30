package com.analytics.model

import java.util.UUID
import java.time.LocalDateTime

/**
  * Package object
  * Group implicits and type aliases
  *
  */
package object core {
  type VisitAggregation = Map[UUID, VisitSummary]

  implicit val dateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)

  type DocumentAggregation = Map[UUID, List[VisitSummary]]
}