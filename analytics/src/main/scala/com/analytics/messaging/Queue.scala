package com.analytics.messaging

import fs2._
import cats.effect.{ContextShift, Sync}
import java.nio.file.Path

import scala.concurrent.ExecutionContext

/**
  * A very general interface for a queue of messages, simply represented
  * as a Stream.
  */
abstract class Queue[F[_], A] {
  def messages: Stream[F, A]
}

object Queue {
  def linesFromFile[F[_] : Sync : ContextShift](path: Path, blockingEC: ExecutionContext, chunkSize: Int): Queue[F, String] =
    new Queue[F, String] {
      def messages =
        io.file
          .readAll[F](path, blockingEC, chunkSize)
          .through(text.utf8Decode)
          .through(text.lines)
    }
}