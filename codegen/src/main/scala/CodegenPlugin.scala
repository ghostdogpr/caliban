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

      runtime.unsafeRun(
        execCommand(args).foldM(
          reason =>
            putStrLn(reason.toString) *>
              putStrLn(reason.getStackTrace.mkString("\n"))
              as (1),
          _ => Task.succeed(1)
        )
      )

      state
    }
  private val helpMsg =
    """
      |codegen schemaPath outPath ?scalafmtPath
      |
      |Command will write a scala file (outPath) containing GraphQL types,
      |queries and subscriptions for provided json schema (schemaPath) and will
      |format generated code with scalafmt with config in (scalafmtPath) or
      |default config provided along with caliban-codegen.
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
      formatted <- fmtPath
                    .map(Generator.format(code, _))
                    .getOrElse(Task(Generator.formatStr(code, ScalaWriter.scalafmtConfig)))
      _ <- Task(new PrintWriter(new File(toPath)))
            .bracket(q => UIO(q.close()), { pw =>
              Task(pw.println(formatted))
            })
    } yield ()
}
