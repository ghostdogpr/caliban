package caliban.codegen

import _root_.caliban.tools.Codegen.GenType
import _root_.caliban.tools._
import zio._
import zio.config.{ read, ConfigDescriptor, ConfigSource, ReadError }
import zio.console._

object Launch extends App {
  def run(args: List[String]) = (for {
    opts  <- ZIO.fromEither(Options.fromArgs(args))
    files <- Codegen.generate(opts, GenType.Client)
  } yield ()).catchSome({ case mv: ReadError[_] => putStrLn(mv.toString()) }).exitCode
}
