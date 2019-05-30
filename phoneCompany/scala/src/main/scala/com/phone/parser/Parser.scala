package com.phone.parser

import cats.effect.{IO, Resource}
import cats.implicits._
import com.phone.model.RawInput

import scala.io._

object Parser {

  def inputStream(path: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable(IO(Source.fromURL(getClass.getResource(path))))

  def parseRawInput(line: String): IO[RawInput] =
    Some(line.split(" ").map(_.trim).toArray)
      .filter(_.size == 3)
      .fold(IO.raiseError[RawInput](new Exception(s"unexpected input from $line")))(
        (cols: Array[String]) => IO.pure(RawInput(cols(0), cols(1), cols(2)))
      )

  def readRawInput(inputStream: BufferedSource): IO[List[RawInput]] =
    inputStream.getLines.toList.filter(_.nonEmpty).traverse(parseRawInput(_))
}