package com.analytics.pipes

import fs2.Pipe

import io.circe.parser.decode
import com.analytics.model.core._
import com.analytics.model.json._
import cats.effect._
import io.circe.generic.auto._
import io.circe.syntax._
import com.analytics.conversions.CirceGenericDerivation._

/**
  * InputParsing
  * Pipes responsible for parsing the input stream from string to a Visit
  *
  */
object InputParsing {

  def parsingInput: Pipe[IO, String, Visit] =
    parsingJson andThen handleParsingErrors

  def parsingJson[F[_]]: Pipe[F, String, Either[io.circe.Error, Message]] =
    in => in.map(decode[Message](_))

  def handleParsingErrors: Pipe[IO, Either[io.circe.Error, Message], Visit] =
    in => in.evalMap(
      (x: Either[io.circe.Error, Message]) => x match {
        case Right(m: Message) => IO.pure(Some(m))
        case Left(e: io.circe.Error) =>
          IO(println(s"Error during parsing: $e")).map(_ => None: Option[Message])
      }
    ).unNone.map(_.visit)
}