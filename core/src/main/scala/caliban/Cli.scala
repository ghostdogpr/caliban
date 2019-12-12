package caliban

import java.io.File
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

  def run(args: List[String]) =
    execCommand(args).fold(q => {
      println(q)
      println(q.getStackTrace.mkString("\n"))
      1
    }, _ => 0)

  def execCommand(args: List[String]) =
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
      file          <- Task(new File(toPath))
      _ <- Task {
            val pw = new java.io.PrintWriter(file)
            try {
              pw.println(formatted)
            } finally {
              pw.close()
            }
          }
    } yield ()

}
