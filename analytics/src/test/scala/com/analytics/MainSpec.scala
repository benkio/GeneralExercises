package com.analytics

import org.scalatest._
import scala.io._
import fs2._
import cats.effect._
import java.nio.file.Paths
import java.util.UUID
import java.time.LocalDateTime
import com.analytics.model.core._

class MainSpec extends WordSpec with Matchers {

  val testSampleInputFilename = "src/test/resources/small-test-visit-messages.log"

  "app" should {
    "return the expected result when the sample input is provided" in {
      val result = Main.app(Paths.get(testSampleInputFilename)).compile.toList.unsafeRunSync
      val expected =
        List(DocumentSummary(UUID.fromString("f9e34776-879f-4bc5-8316-c2bb68da9cc1"),
                             LocalDateTime.parse("2015-05-18T23:55:49.287"),
                             LocalDateTime.parse("2015-05-18T23:55:49.287"),
                             1,
                             1,
                             0,
                             0.0),
             DocumentSummary(UUID.fromString("088dc30d-1634-4f0d-89c3-764172824a98"),
                             LocalDateTime.parse("2015-05-18T23:55:49.288"),
                             LocalDateTime.parse("2015-05-18T23:55:49.288"),
                             1,
                             1,
                             0,
                             0.0),
             DocumentSummary(UUID.fromString("faa8524a-0633-4cdf-9284-1241084aef09"),
                             LocalDateTime.parse("2015-05-18T23:55:50.141"),
                             LocalDateTime.parse("2015-05-18T23:56:30.002"),
                             1,
                             1,
                             35,
                             0.3795272395252105),
             DocumentSummary(UUID.fromString("b61d8914-560f-4985-8a5f-aa974ad0c7ab"),
                             LocalDateTime.parse("2015-05-18T23:55:49.254"),
                             LocalDateTime.parse("2015-05-18T23:56:54.357"),
                             1,
                             1,
                             35,
                             0.29763610315186245))
      result shouldEqual expected
    }
  }

}