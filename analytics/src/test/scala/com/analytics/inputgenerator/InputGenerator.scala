package com.analytics.inputgenerator

import com.analytics.model.core._
import com.analytics.model.json._
import com.analytics.pipes.VisitAggregation
import java.time.LocalDateTime
import java.util.UUID
import org.scalacheck._
import java.time.format.DateTimeFormatter
import cats._

object InputGenerator {
  type GenList[A] = Gen[List[A]]

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")

  val generateJVisitCreateJson: Gen[String] = for {
    id <- Gen.uuid
    userId <- Gen.uuid
    documentId <- Gen.uuid
    createdAt <- localDateTime()
  } yield
      s"""{
  "messageType" : "VisitCreate",
  "visit" : {
    "id" : "$id",
    "userId" : "$userId",
    "documentId" : "$documentId",
    "createdAt" : "${createdAt.format(dateFormatter)}"
  }
}"""

  val generateJVisitUpdateJson: Gen[String] = for {
    id <- Gen.uuid
    engagedTime <- Gen.choose(1, 100)
    completion <- Gen.choose(0d, 100d).map(BigDecimal.valueOf)
    updatedAt <- localDateTime()
  } yield
      s"""{
  "messageType" : "VisitUpdate",
  "visit" : {
    "id" : "$id",
    "engagedTime" : $engagedTime,
    "completion" : $completion,
    "updatedAt" : "${updatedAt.format(dateFormatter)}"
  }
}"""

  val generateJVisitCreate: Gen[JVisitCreate] =
    for {vc <- generateVisitCreate(None)} yield JVisitCreate(vc)

  val generateJVisitUpdate: Gen[JVisitUpdate] =
    for {vu <- generateVisitUpdate(None)} yield JVisitUpdate(vu)


  val genListSemigroupK: SemigroupK[GenList] = new SemigroupK[GenList] {
    def combineK[A](gen1: GenList[A], gen2: GenList[A]): GenList[A] = for {
      l1 <- gen1
      l2 <- gen2
    } yield l1 ++ l2
  }


  val validStreamGenerator: Gen[List[Visit]] = for {
    listSize <- Gen.posNum[Int]
    singleVisitStream <- List.fill(listSize)(UUID.randomUUID.toString)
    .map((id: String) => InputGenerator.singleVisitStreamGenerator(id))
    .fold(Gen.const(List()): Gen[List[Visit]])(genListSemigroupK.combineK)
  } yield singleVisitStream


  val generateVisitAggregation: Gen[Map[UUID, VisitSummary]] = for {
    visitStream <- validStreamGenerator
  } yield visitStream
    .foldLeft(Map.empty[UUID, VisitSummary])(
      (visitAggregator: Map[UUID, VisitSummary], visit: Visit) =>
      VisitAggregation.aggregateVisit(visitAggregator,
                                      visit).getOrElse(visitAggregator)
    )

  def singleVisitStreamGenerator(visitId: String): Gen[List[Visit]] = {
    for {
      vu <- Gen.listOf(generateVisitUpdate(Some(Gen.const(visitId))))
      vc <- generateVisitCreate(Some(Gen.const(visitId)))
    } yield vc :: vu
  }

  def generateVisitCreate(id: Option[Gen[String]] = None): Gen[VisitCreate] = for {
    id <- id.map(_.map(UUID.fromString)).getOrElse(Gen.uuid)
    userId <- Gen.uuid
    documentId <- Gen.uuid
    createdAt <- localDateTime()
  } yield VisitCreate(id, userId, documentId, createdAt)

  def localDateTime(minRange: Range = Range(0, 59),
                    hourRange: Range = Range(0, 23),
                    dayRange: Range = Range(1, 28),
                    monthRange: Range = Range(1, 12),
                    year: Option[Int] = None
  ): Gen[LocalDateTime] = for {
    min <- Gen.choose(minRange.start, minRange.end)
    hour <- Gen.choose(hourRange.start, hourRange.end)
    day <- Gen.choose(dayRange.start, dayRange.end)
    month <- Gen.choose(monthRange.start, monthRange.end)
    y <- year.fold(Gen.choose(0, 2000))(Gen.const(_))
  } yield LocalDateTime.of(y, month, day, hour, min)

  def generateVisitUpdate(id: Option[Gen[String]] = None): Gen[VisitUpdate] = for {
    id <- id.map(_.map(UUID.fromString)).getOrElse(Gen.uuid)
    engagedTime <- Gen.choose(1, 100)
    completion <- Gen.choose(0d, 100d).map(BigDecimal.valueOf)
    updatedAt <- localDateTime()
  } yield VisitUpdate(id, engagedTime, completion, updatedAt)

  def generateSingleVisitSummary : Gen[VisitSummary] = for {
    uuid <- Gen.uuid
    visitCreateUpdate @ (vc : VisitCreate) :: (lvu : List[VisitUpdate]) <- singleVisitStreamGenerator(uuid.toString)

  } yield lvu.foldRight(VisitSummary(vc))(VisitSummary.applyUpdate)

  def generateBaseDocumentSummary : Gen[DocumentSummary] = for {
    uuid <- Gen.uuid
    startTime<- InputGenerator.localDateTime()
    endTime <- InputGenerator.localDateTime()
    uniques <- Gen.posNum[Int]
  } yield DocumentSummary.init(uuid, startTime, endTime, uniques)
}
