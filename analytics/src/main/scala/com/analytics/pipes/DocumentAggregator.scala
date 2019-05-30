package com.analytics.pipes

import fs2.{Pipe, Stream}
import com.analytics.model.core._

/**
  * DocumentAggregation
  * Pipes responsible for the aggregation of Visits to Documents
  *
  */
object DocumentAggregation {

  def documentAggregationPipe[F[_]] : Pipe[F, VisitAggregation, DocumentSummary] =
    in => in
      .through(documentGroupingPipe)
      .flatMap((documentSummaries : List[DocumentSummary]) =>
        Stream.emits(documentSummaries)
      )

  def documentGroupingPipe[F[_]] : Pipe[F, VisitAggregation, List[DocumentSummary]] =
    in => in.map(foldDocumentAggregation _)

  def foldDocumentAggregation(visitAggregation : VisitAggregation) : List[DocumentSummary] =
    DocumentSummary(visitAggregation.values.toList)

}