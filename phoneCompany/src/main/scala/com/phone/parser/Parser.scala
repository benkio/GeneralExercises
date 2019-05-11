package com.phone.parser

import cats.effect.{IO, Resource}
import java.io.{File, FileInputStream}
import com.phone.model.RawInput

object Parser {

  def inputStream(f: File): Resource[IO, FileInputStream] = ???

  def readRawInput(inputStream : FileInputStream) : IO[List[RawInput]] = ???
}