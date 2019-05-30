package com.analytics.conversions

import io.circe._
import io.circe.Decoder._
import java.time.LocalDateTime
import scala.util.control.NonFatal
import java.time.format.DateTimeFormatter
import com.analytics.model.json._

/**
  * CirceGenericDerivation
  * Group together custom parsers
  *
  */
object CirceGenericDerivation {
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  implicit val decodeLocalDateTime: Decoder[LocalDateTime] = Decoder.decodeString.emap { s =>
    try {
      Right(LocalDateTime.parse(s, dateFormatter))
    } catch {
      case NonFatal(e) => Left(e.getMessage)
    }
  }

  implicit def messageDecoder(
    implicit decodeJVisitCreate: Decoder[JVisitCreate],
    decodeJVisitUpdate: Decoder[JVisitUpdate]
  ): Decoder[Message] = new Decoder[Message] {
    override def apply(cursor: HCursor): Result[Message] = cursor.downField("messageType").as[String] match {
      case Right("VisitCreate") => decodeJVisitCreate(cursor)
      case Right("VisitUpdate") => decodeJVisitUpdate(cursor)
      case x => Left(DecodingFailure("unrecognized message", cursor.history))
    }
  }
}