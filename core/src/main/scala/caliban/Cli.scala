package caliban

import java.io.File
import caliban.modelgen.{ Generator, ScalaWriter }
import caliban.parsing.Parser
import zio._
import zio.console._

object Cli extends App {
  val printHelp = putStrLn(
    """
    schema      (schema_path, out_path)     will write a scala object containing types, queries, fragments etc. for provided schema
  """
  )

  def run(args: List[String]) =
    execCommand(args).fold(q => {
      println(q)
      println(q.getStackTrace.mkString("\n"))
      1
    }, _ => 0)

  def execCommand(args: List[String]) = args match {
    case "schema" :: schema_path :: to_path :: Nil => doSchemaGenerate(schema_path, to_path)
    case _                                         => printHelp
  }

  def doSchemaGenerate(schema_path: String, to_path: String) =
    for {
      schema_string <- Task(scala.io.Source.fromFile(schema_path).mkString)
      schema        <- Parser.parseQuery(schema_string)
      code          <- Task(Generator.generate(schema)(ScalaWriter.DefaultGQLWriter))
      formatted     <- Task(Generator.format(code))
      file          <- Task(new File(to_path))
      _ <-  Task {
              val pw = new java.io.PrintWriter(file)
              try {
                pw.println(formatted)
              } finally {
                pw.close()
              }
            }
    } yield ()

}
