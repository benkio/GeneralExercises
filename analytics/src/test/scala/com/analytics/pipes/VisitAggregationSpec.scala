package com.analytics.pipes

import java.util.UUID

import com.analytics.model.core._
import fs2._
import java.time._
import org.scalacheck._
import org.scalacheck.Prop._
import com.analytics.inputgenerator._

class VisitAggregationSpec extends Properties("VisitAggregation") {

  property("aggregateVisit should return a None when the input map is empty and message is an update") = forAll(InputGenerator.generateVisitUpdate()) {
    (vu: VisitUpdate) =>
    VisitAggregation.aggregateVisit(Map.empty[UUID, VisitSummary],
                                    vu).isEmpty
  }

  property("aggregateVisit should return a Some when the input map is empty and message is a create") = forAll(InputGenerator.generateVisitCreate()) {
    (vc: VisitCreate) => {
      val result = VisitAggregation.aggregateVisit(Map.empty[UUID, VisitSummary],
                                                   vc)
      result.isDefined &&
        result.flatMap(_.get(vc.id)).isDefined &&
        result.flatMap(_.get(vc.id)).get == VisitSummary(vc)
    }
  }

  property("aggregateVisit should return a None when the input map already contains the uuid of the input create message") = forAll(InputGenerator.generateVisitCreate()) {
    (vc: VisitCreate) =>
    VisitAggregation.aggregateVisit(Map(vc.id -> VisitSummary(vc)),
                                    vc).isEmpty
  }

  property("aggregateVisit should return a Some when the input map already contains the uuid of the input update message") = forAll(InputGenerator.generateVisitUpdate(), InputGenerator.generateVisitCreate()) {
    (vu: VisitUpdate, vc: VisitCreate) =>
    val result = VisitAggregation.aggregateVisit(Map(vc.id -> VisitSummary(vc)),
                                                 vu.copy(id = vc.id))
    result.isDefined &&
      result.flatMap(_.get(vc.id)).isDefined &&
      result.flatMap(_.get(vc.id)).get == VisitSummary.applyUpdate(vu, VisitSummary(vc))
  }

  property("aggregateVisit should return a Some when the input map is not empty but doesn't contains the uuid of the input create message") = forAll(InputGenerator.generateVisitCreate(), Gen.uuid) {
    (vc: VisitCreate, genericUUID: UUID) =>
    val result = VisitAggregation.aggregateVisit(Map(genericUUID -> VisitSummary(vc)),
                                                 vc)
    result.isDefined &&
      result.get.size == 2 &&
      result.flatMap(_.get(vc.id)).get == VisitSummary(vc)
  }

  property("aggregateVisit should return a None when the input map is not empty but doesn't cotain the id of the input update") = forAll(InputGenerator.generateVisitUpdate(), Gen.uuid, InputGenerator.generateVisitCreate()) {
    (vu: VisitUpdate, genericUUID: UUID, vc: VisitCreate) =>
    VisitAggregation.aggregateVisit(Map(genericUUID -> VisitSummary(vc)),
                                    vu).isEmpty
  }

  property("filterAggregatedVisit should return an empty Map if the input is an empty map") = forAll(Gen.const(Map.empty[UUID, VisitSummary])) {
    (inputMap: Map[UUID, VisitSummary]) => VisitAggregation.filterAggregatedVisit(inputMap, Duration.ofMinutes(5), LocalDateTime.of(1999, 1, 1, 1, 1)).isEmpty
  }

  implicit val dateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)
  property("filterAggregatedVisit should return an empty Map if the input duration is ZERO") = forAll(InputGenerator.validStreamGenerator) {
    (validStream: List[Visit]) => {
      val inputMap = validStream.foldLeft(Map.empty[UUID, VisitSummary])(
        (visitAggregation : Map[UUID, VisitSummary], v : Visit) =>
        VisitAggregation.aggregateVisit(visitAggregation,
                                        v).getOrElse(visitAggregation)
      )
      val mostRecentVisitTime = inputMap
        .values
        .map((vs: VisitSummary) => vs.lastUpdate.getOrElse(vs.createdAt)).toList.sorted.reverse.head
      val result = VisitAggregation.filterAggregatedVisit(inputMap, Duration.ZERO, mostRecentVisitTime)
      result.size == 0 || result.size == 1
    }
  }

  property("filterAggregatedVisit should remove all the visit summaries where the last update is before the Max(input datetime, Max(filterAggregatedVisitTime)) - input duration") = forAll(InputGenerator.generateVisitAggregation) {
    (inputMap: Map[UUID, VisitSummary]) => {
      val visitTimes = inputMap
        .values
        .map(_.newestUpdate).toList.sorted
      if( visitTimes.size > 0 ){
        val inputLocalDateTime = visitTimes(visitTimes.size / 2)
        VisitAggregation.filterAggregatedVisit(inputMap, Duration.ofSeconds(1), inputLocalDateTime).size == 1
      } else true
    }
  }

  property("aggregateVisitPipe should return always an empty list of visit failures if the stream is valid") = forAll(InputGenerator.validStreamGenerator) {
    (validStream: List[Visit]) => {
      val result: List[(VisitAggregation, List[Visit])] =
        Stream.emits(validStream).through(VisitAggregation.aggregateVisitPipe[Pure](Duration.ofDays(10000000))).compile.toList
      result.forall((x: (VisitAggregation, List[Visit])) => x._2.isEmpty)
    }
  }

  property("aggregateVisitPipe should return a map containing just the last visit if the validVisitWindow is Zero") = forAll(InputGenerator.validStreamGenerator) {
    (validStream : List[Visit]) => {
      val result: List[(VisitAggregation, List[Visit])] =
        Stream.emits(validStream.sortBy(_.time)).through(VisitAggregation.aggregateVisitPipe[Pure](Duration.ZERO)).compile.toList
      val visitSummaries = result.last._1.values
      visitSummaries.size match {
        case 1 => {
          val lastVisitSummaryTime = visitSummaries.head.lastUpdate.getOrElse(visitSummaries.head.createdAt)
          val lastVisitSummariesKey = result.last._1.keySet.head
          val expectedLastVisitSummaryTime = validStream.filter(_.id == lastVisitSummariesKey).map(_.time).sorted.last
          lastVisitSummaryTime == expectedLastVisitSummaryTime
        }
        case 0 => true
        case _ => false
      }
    }
  }

  property("aggregateVisit should return a map containing all the keys in the valid input stream") = forAll(InputGenerator.validStreamGenerator) {
    (validStream: List[Visit]) => {
      val result: VisitAggregation =
        Stream.emits(validStream).through(VisitAggregation.aggregateVisit(Duration.ofDays(10000000))).compile.toList.unsafeRunSync.last
      result.keySet.diff(validStream.map(_.id).toSet).isEmpty
    }
  }
}
