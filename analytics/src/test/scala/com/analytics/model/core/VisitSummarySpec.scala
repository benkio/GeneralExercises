package com.analytics.model.core

import java.time.LocalDateTime
import java.util.UUID
import org.scalacheck._
import org.scalacheck.Prop._
import com.analytics.inputgenerator._

class VisitSummarySpec extends Properties("VisitSummary") {

  val baseVisitSummary = VisitSummary(VisitCreate(UUID.fromString("4f7e8f64-8baf-44c2-912b-e3364549c25e"),
                                                  UUID.fromString("c4bfbbdd-6f42-45d5-8eb9-cab4fe71349e"),
                                                  UUID.fromString("3f53197e-0585-42d3-8d14-178b21d9abbc"),
                                                  LocalDateTime.of(1999, 1, 1, 1, 1)
                                      ))

  property("apply creates an empty visitSummary from VisitCreate") = forAll(InputGenerator.generateVisitCreate()) {
    (vc: VisitCreate) => {
      val result: VisitSummary = VisitSummary(vc)
      result.documentId == vc.documentId &&
        result.userId == vc.userId &&
        result.createdAt == vc.createdAt &&
        result.lastUpdate.isEmpty &&
        result.time == 0 &&
        result.completion == 0.0
    }
  }

  property("applyUpdate changes the expected fields") = forAll(InputGenerator.generateVisitUpdate()) {
    (vu: VisitUpdate) => {
      val result = VisitSummary.applyUpdate(vu,
                                            baseVisitSummary)
      result.documentId == baseVisitSummary.documentId &&
        result.userId == baseVisitSummary.userId &&
        result.createdAt == baseVisitSummary.createdAt &&
        result.lastUpdate == Some(vu.updatedAt) &&
        result.time == vu.engagedTime &&
        result.completion == vu.completion
    }
  }
}