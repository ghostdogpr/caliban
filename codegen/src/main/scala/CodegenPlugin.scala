package codegen

import java.io.{ File, PrintWriter }

import sbt._
import Keys._
import caliban.parsing.Parser
import zio.console.Console
import zio.{ DefaultRuntime, Task, UIO, ZIO }

object CodegenPlugin extends AutoPlugin {
  import Console.Live.console._
  override lazy val projectSettings = Seq(commands += codegenCommand)
  lazy val codegenCommand =
    Command.args("codegen", helpMsg) { (state: State, args: Seq[String]) =>
      val runtime = new DefaultRuntime {}

      runtime.unsafeRun(execCommand(args).fold(q => {
        putStrLn(q.toString)
        putStrLn(q.getStackTrace.mkString("\n"))
        1
      }, _ => 0))

      state
    }
  private val helpMsg =
    """
      |codegen schemaPath outPath ?scalafmtPath
      |
      |Command will write a scala file (outPath) containing GraphQL types,
      |queries and subscriptions for provided schema (schemaPath) and will
      |eventually format generated code with scalafmt (scalafmtPath)
      |
      |""".stripMargin

  def execCommand(args: Seq[String]): ZIO[Console, Throwable, Unit] =
    args match {
      case schemaPath :: toPath :: Nil => doSchemaGenerate(schemaPath, toPath, None)
      case schemaPath :: toPath :: formatPath :: Nil =>
        doSchemaGenerate(schemaPath, toPath, Some(formatPath))
      case _ => putStrLn(helpMsg)
    }

  def doSchemaGenerate(schemaPath: String, toPath: String, fmtPath: Option[String]): ZIO[Console, Throwable, Unit] =
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
