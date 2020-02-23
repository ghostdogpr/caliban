package caliban.codegen

import java.io.{ File, PrintWriter }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import sbt.Keys.commands
import sbt.{ AutoPlugin, Command, State }
import zio.console.Console
import zio.{ DefaultRuntime, RIO, Task, UIO }

object CodegenPlugin extends AutoPlugin {
  import Console.Live.console._
  override lazy val projectSettings = Seq(commands ++= Seq(genSchemaCommand, genClientCommand))
  lazy val genSchemaCommand         = genCommand("calibanGenSchema", genSchemaHelpMsg, SchemaWriter.write)
  lazy val genClientCommand         = genCommand("calibanGenClient", genClientHelpMsg, ClientWriter.write)

  def genCommand(name: String, helpMsg: String, writer: (Document, String, Option[String]) => String): Command =
    Command.args(name, helpMsg) { (state: State, args: Seq[String]) =>
      val runtime = new DefaultRuntime {}
      runtime.unsafeRun(
        execGenCommand(helpMsg, args, writer)
          .catchAll(reason => putStrLn(reason.toString) *> putStrLn(reason.getStackTrace.mkString("\n")))
          .as(1)
      )
      state
    }

  private val genSchemaHelpMsg =
    """
      |calibanGenSchema schemaPath outputPath ?scalafmtPath
      |
      |This command will create a Scala file in `outputPath` containing all the types
      |defined in the provided GraphQL schema defined at `schemaPath`. The generated 
      |code will be formatted with Scalafmt using the configuration defined by
      |`scalafmtPath` (default: ".scalafmt.conf").
      |
      |""".stripMargin

  private val genClientHelpMsg =
    """
      |calibanGenClient schemaPath outputPath ?scalafmtPath
      |
      |This command will create a Scala file in `outputPath` containing client code for all the
      |typed defined in the provided GraphQL schema defined at `schemaPath`. The generated 
      |code will be formatted with Scalafmt using the configuration defined by
      |`scalafmtPath` (default: ".scalafmt.conf").
      |
      |""".stripMargin

  def execGenCommand(
    helpMsg: String,
    args: Seq[String],
    writer: (Document, String, Option[String]) => String
  ): RIO[Console, Unit] =
    args match {
      case schemaPath :: toPath :: Nil               => generate(schemaPath, toPath, None, writer)
      case schemaPath :: toPath :: formatPath :: Nil => generate(schemaPath, toPath, Some(formatPath), writer)
      case _                                         => putStrLn(helpMsg)
    }

  def generate(
    schemaPath: String,
    toPath: String,
    fmtPath: Option[String],
    writer: (Document, String, Option[String]) => String
  ): RIO[Console, Unit] =
    for {
      _             <- putStrLn(s"Generating code for $schemaPath")
      s             = ".*/scala/(.*)/(.*).scala".r.findFirstMatchIn(toPath)
      packageName   = s.map(_.group(1).split("/").mkString("."))
      objectName    = s.map(_.group(2)).getOrElse("Client")
      schema_string <- Task(scala.io.Source.fromFile(schemaPath)).bracket(f => UIO(f.close()), f => Task(f.mkString))
      schema        <- Parser.parseQuery(schema_string)
      code          = writer(schema, objectName, packageName)
      formatted     <- Formatter.format(code, fmtPath)
      _ <- Task(new PrintWriter(new File(toPath))).bracket(q => UIO(q.close()), { pw =>
            Task(pw.println(formatted))
          })
      _ <- putStrLn(s"Code generation done")
    } yield ()

}
