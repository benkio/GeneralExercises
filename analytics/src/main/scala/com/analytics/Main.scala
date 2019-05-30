package com.analytics

import java.nio.file.{Path, Paths}
import java.util.concurrent.Executors

import cats.effect._
import cats.implicits._
import com.analytics.messaging.Queue
import fs2._
import com.analytics.pipes.InputParsing._
import com.analytics.pipes.VisitAggregation._
import com.analytics.pipes.DocumentAggregation._
import com.analytics.constants.TimeConstants._
import com.analytics.model.core.DocumentSummary
import scala.concurrent.ExecutionContext

object Main extends IOApp {
  private val blockingExecutionContext =
    Resource.make(IO(
                    ExecutionContext.fromExecutorService(
                      Executors.newFixedThreadPool(2))))(ec => IO(ec.shutdown()))

  def run(args: List[String]): IO[ExitCode] = {
    val path =
      args.headOption
        .map(s => Paths.get(s))
        .getOrElse(throw new Exception("Please provide a valid input file"))

    app(path)
      .evalMap[IO, Unit](s => IO {println("Received message: " + s)})
      .compile.drain.as(ExitCode.Success)
  }

  def app(path: Path) : Stream[IO, DocumentSummary] =
    Stream.resource(blockingExecutionContext).flatMap { blockingEC =>
      Queue.linesFromFile[IO](path, blockingEC, 4096).messages
        .through(parsingInput)
        .through(aggregateVisit(validVisitWindow))
        .debounce(aggregatedTimeWindow)
        .through(documentAggregationPipe)

    }
}