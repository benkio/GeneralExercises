package com.analytics.model.core

import java.time.LocalDateTime
import java.util.UUID

/**
  * Visit
  * Trait used to group together the input messages once parsed
  *
  */
sealed trait Visit {
  def id: UUID
  def time : LocalDateTime
}

/**
  * VisitCreate
  * Represent the VisitCreate Message parsed
  *
  */
case class VisitCreate(id: UUID,
                       userId: UUID,
                       documentId: UUID,
                       createdAt: LocalDateTime) extends Visit{
  def time : LocalDateTime = createdAt
}

/**
  * VisitUpdate
  * Represent the VisitUpdate Message parsed
  *
  */
case class VisitUpdate(id: UUID,
                       engagedTime: Int,
                       completion: BigDecimal,
                       updatedAt: LocalDateTime) extends Visit {
  def time : LocalDateTime = updatedAt
}