package com.analytics.pipes

import org.scalacheck._
import org.scalacheck.Prop._
import com.analytics.inputgenerator._
import com.analytics.model.json.Message
import fs2.Stream

class MessagesSpec extends Properties("InputParsing") {

  property("parsingJson decodes right input") = forAll(Gen.listOfN(5, Gen.oneOf(InputGenerator.generateJVisitUpdateJson,
                                                                                InputGenerator.generateJVisitCreateJson))) {
    (jsons: List[String]) => {
      val result = Stream.emits(jsons.toSeq).through(InputParsing.parsingJson).compile.toList
      result.size == jsons.size && result.forall(_.isRight)
    }
  }

  property("handleParsingErrors on right input") = forAll(Gen.listOfN(5, Gen.oneOf(InputGenerator.generateJVisitCreate,
                                                                                   InputGenerator.generateJVisitUpdate).map(Right(_)))) {
    (messages: List[Either[io.circe.Error, Message]]) => {
      val result = Stream.emits(messages.toSeq).through(InputParsing.handleParsingErrors).compile.toList.unsafeRunSync()
      result.size == messages.size && result == messages.map(_.right.get.visit)
    }
  }

  property("parsingInput decodes right input") = forAll(Gen.listOfN(5, Gen.oneOf(InputGenerator.generateJVisitUpdateJson,
                                                                                 InputGenerator.generateJVisitCreateJson))) {
    (jsons: List[String]) => {
      val result = Stream.emits(jsons.toSeq).through(InputParsing.parsingInput).compile.toList.unsafeRunSync
      result.size == jsons.size
    }
  }
}