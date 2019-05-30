package com.analytics.model.core

import java.time.LocalDateTime
import java.util.UUID

/**
  * VisitSummary
  * This case class is the merge of visits by ID
  *
  */
case class VisitSummary(documentId: UUID,
                        userId: UUID,
                        createdAt: LocalDateTime,
                        lastUpdate: Option[LocalDateTime],
                        time: Int,
                        completion: BigDecimal) {
  def newestUpdate : LocalDateTime = lastUpdate.getOrElse(createdAt)
}

object VisitSummary {
  /**
    * Can create VisitSummary only from a VisitCreate
    */
  def apply(visitCreate: VisitCreate): VisitSummary =
    VisitSummary(visitCreate.documentId,
                 visitCreate.userId,
                 visitCreate.createdAt,
                 None,
                 0,
                 0.0)

  /**
    * Can Update the VisitSummary only with a VisitUpdate
    */
  def applyUpdate(visitUpdate: VisitUpdate,
                  visitSummary: VisitSummary): VisitSummary =
    visitSummary.copy(lastUpdate = Some(visitUpdate.updatedAt),
                      time = visitUpdate.engagedTime,
                      completion = visitUpdate.completion)
}