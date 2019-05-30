package com.analytics.model.core

import java.time.LocalDateTime
import java.util.UUID

/**
  * Final output of the program. Aggregates everything by documentId
  */
case class DocumentSummary(documentId : UUID,
                           startTime: LocalDateTime,
                           endTime : LocalDateTime,
                           visits: Int,
                           uniques: Int,
                           time: Int,
                           completion: BigDecimal)

object DocumentSummary {

  /**
    * Creates a list of DocumentSummary from a list of VisitSummary.
    * It first groupBy documentId and then fold the content into the
    * documentSummary
    */
  def apply(visitSummaries: List[VisitSummary]) : List[DocumentSummary] =
    visitSummaries
      .groupBy(_.documentId)
      .map { case (dId : UUID, vs : List[VisitSummary]) =>{
              val minTime = vs.minBy(_.createdAt).createdAt
              val maxTime = vs.maxBy(_.newestUpdate).newestUpdate
              val uniques = vs.map(_.userId).distinct.size
              vs.foldLeft(DocumentSummary.init(dId,
                                               minTime,
                                               maxTime,
                                               uniques))(applyUpdate)
            }
      }.toList

  /**
    * Allow to create a DocumentSummary with the non-aggregated informations.
    */
  def init(documentId : UUID,
           startTime : LocalDateTime,
           endTime : LocalDateTime,
           uniques : Int) : DocumentSummary =
    DocumentSummary(documentId,
                    startTime,
                    endTime,
                    0,
                    uniques,
                    0,
                    0.0)

  /**
    * Add to the DocumentSummary the data from the input VisitSummary
    */
  def applyUpdate(dc : DocumentSummary,
                  vs : VisitSummary) : DocumentSummary = {
    dc.copy(visits = dc.visits + 1,
            time = dc.time + vs.time,
            completion = dc.completion + vs.completion)
  }
}