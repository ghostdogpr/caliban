package caliban

import java.io.{ File, PrintWriter }

import caliban.codegen.{ Generator, ScalaWriter }
import caliban.parsing.Parser
import zio._
import zio.console._

object Cli extends App {
  val printHelp = putStrLn(
    """
    schema      (schema_path, out_path, ?scalafmt_path)     will write a scala object containing types, queries, fragments etc. for provided schema
  """
  )

  def run(args: List[String]): URIO[Console, Int] =
    execCommand(args).fold(q => {
      putStrLn(q.toString)
      putStrLn(q.getStackTrace.mkString("\n"))
      1
    }, _ => 0)

  def execCommand(args: List[String]): ZIO[Console, Throwable, Unit] =
    args match {
      case "schema" :: schemaPath :: toPath :: Nil => doSchemaGenerate(schemaPath, toPath, None)
      case "schema" :: schemaPath :: toPath :: formatPath :: Nil =>
        doSchemaGenerate(schemaPath, toPath, Some(formatPath))
      case _ => printHelp
    }

  def doSchemaGenerate(schemaPath: String, toPath: String, fmtPath: Option[String]) =
    for {
      schema_string <- Task(scala.io.Source.fromFile(schemaPath).mkString)
      schema        <- Parser.parseQuery(schema_string)
      code          <- Task(Generator.generate(schema)(ScalaWriter.DefaultGQLWriter))
      formatted     <- fmtPath.map(Generator.format(code, _)).getOrElse(Task.succeed(code))
      _ <- Task(new PrintWriter(new File(toPath)))
            .bracket(q => UIO(q.close()), { pw =>
              Task(pw.println(formatted))
            })
    } yield ()

}
