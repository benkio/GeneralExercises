package com.analytics.model.core

import java.time.LocalDateTime
import java.util.UUID
import org.scalacheck._
import org.scalacheck.Prop._
import com.analytics.inputgenerator._

class DocumentSummarySpec extends Properties("DocumentSummary") {

  property("init should update only the required fields ad set 0 the others") =
    forAll(Gen.uuid,
           InputGenerator.localDateTime(),
           InputGenerator.localDateTime(),
           Gen.posNum[Int]) {
      (id : UUID, startTime : LocalDateTime, endTime : LocalDateTime, uniques : Int) =>{
        val result = DocumentSummary.init(id, startTime, endTime, uniques)
        result.documentId == id &&
          result.startTime == startTime &&
          result.endTime == endTime &&
          result.visits == 0 &&
          result.uniques == uniques &&
          result.time == 0 &&
          result.completion == 0.0
      }
    }

  property("applyUpdate should update only the required fields") = forAll(InputGenerator.generateSingleVisitSummary,
                                                                          InputGenerator.generateBaseDocumentSummary) {
    (vs : VisitSummary, ds : DocumentSummary) => {
      val result = DocumentSummary.applyUpdate(ds, vs)
      result.documentId == ds.documentId &&
        result.startTime == ds.startTime &&
        result.endTime == ds.endTime &&
        result.visits == ds.visits + 1 &&
        result.uniques == ds.uniques &&
        result.time == ds.time + vs.time &&
        result.completion == ds.completion + vs.completion
    }
  }

  property("apply should sum the data in input into the DocumentSummary") =
    forAll(Gen.listOf(InputGenerator.generateSingleVisitSummary)) {
      (lvs : List[VisitSummary]) => {
        val result = DocumentSummary(lvs)
        result.size == lvs.map(_.documentId).distinct.size &&
          result.map(_.time).sum == lvs.map(_.time).sum &&
          result.map(_.completion).sum == lvs.map(_.completion).sum &&
          result.map(_.visits).sum == lvs.size
      }
    }
}
