package com.analytics.pipes

import com.analytics.model.core._
import com.analytics.pipes.DocumentAggregation._
import org.scalacheck._
import org.scalacheck.Prop._
import com.analytics.inputgenerator._

class DocumentAggregationSpec extends Properties("DocumentAggregation") {

  property("foldDocumentAggregation should preseve the document ids into the output map keys") =
    forAll(InputGenerator.generateVisitAggregation) {
      (visitAggregation : VisitAggregation) => {
        val result = foldDocumentAggregation(visitAggregation)
        result.map(_.documentId).diff(
          visitAggregation
            .values
            .toList
            .map((vs : VisitSummary) => vs.documentId)
        ).isEmpty
      }
    }
}
