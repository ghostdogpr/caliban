package caliban.codegen

import _root_.caliban.tools.Codegen.GenType
import _root_.caliban.tools._
import java.nio.file.{ Path, Paths }
import zio._
import zio.config.{ read, ConfigDescriptor, ConfigSource, ReadError }
import zio.console._

case class MissingGenType(args: List[String]) extends NoSuchElementException {
  override def toString() = s"MissingGenType(${args.mkString(" ")})"
}

object Launch extends App {
  val extractType: List[String] => IO[NoSuchElementException, (GenType, List[String])] = {
    case "client" +: rest => UIO((GenType.Client, rest))
    case "schema" +: rest => UIO((GenType.Schema, rest))
    case args             => ZIO.fail(MissingGenType(args))
  }

  def mkDirs(path: Path): Task[Unit] =
    Task(path.toFile.mkdirs).as(())

  def run(args: List[String]) = (for {
    (tpe, rest) <- extractType(args)
    opts        <- ZIO.fromEither(Options.fromArgs(rest))
    _           <- mkDirs(Paths.get(opts.toPath).getParent)
    files       <- Codegen.generate(opts, tpe)
  } yield ())
    .catchSome({
      case mv: ReadError[_]     => putStrLn(mv.toString())
      case MissingGenType(args) => putStrLn("cli {client|schema} [opts [opts...]]")
    })
    .exitCode
}
