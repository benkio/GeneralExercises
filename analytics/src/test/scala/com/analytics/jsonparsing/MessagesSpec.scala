package com.analytics.jsonparsing

import io.circe.generic.auto._, io.circe.syntax._
import com.analytics.model.core._
import com.analytics.model.json._
import com.analytics.conversions.CirceGenericDerivation._
import io.circe.parser.decode
import org.scalacheck._
import org.scalacheck.Prop._
import com.analytics.inputgenerator._

class MessagesSpec extends Properties("Messages") {

  property("decoding JVisitCreate") = forAll(InputGenerator.generateJVisitCreateJson) {
    (json: String) => decode[JVisitCreate](json).isRight == true
  }
  property("decoding JVisitUpdate") = forAll(InputGenerator.generateJVisitUpdateJson) {
    (json: String) => decode[JVisitUpdate](json).isRight == true
  }
  property("decoding messages") = forAll(Gen.oneOf(InputGenerator.generateJVisitUpdateJson,
                                                   InputGenerator.generateJVisitCreateJson)) {
    (json: String) => decode[Message](json).isRight == true
  }
}