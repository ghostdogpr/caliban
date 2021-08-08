package caliban.codegen

import _root_.caliban.tools.Codegen.GenType
import _root_.caliban.tools._
import zio._
import zio.console._

object Launch extends App {
  def run(args: List[String]) = (for {
    opts  <- ZIO.fromOption(Options.fromArgs(args))
    files <- Codegen.generate(opts, GenType.Client).asSomeError
  } yield ()).exitCode
}
