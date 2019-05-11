package com.phone

import cats.effect._

object Main extends IOApp {

  def run(args : List[String]) : IO[ExitCode] =
    args.headOption match {
      case Some(name) =>
        IO(println(s"Hello, $name.")).map(_ => ExitCode.Success)
      case None =>
        IO(System.err.println("Usage: MyApp name")).map(_ => ExitCode(2))
    }
}
