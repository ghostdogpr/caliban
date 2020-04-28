package caliban.codegen

import caliban.parsing.adt.Document
import sbt.Keys.commands
import sbt.{ AutoPlugin, Command, State }
import zio.console.{ putStrLn, Console }
import zio.{ RIO, Runtime }

object CodegenPlugin extends AutoPlugin {
  override lazy val projectSettings = Seq(commands ++= Seq(genSchemaCommand, genClientCommand))
  lazy val genSchemaCommand         = genCommand("calibanGenSchema", genSchemaHelpMsg, SchemaWriter.write)
  lazy val genClientCommand         = genCommand("calibanGenClient", genClientHelpMsg, ClientWriter.write)

  def genCommand(name: String, helpMsg: String, writer: (Document, String, Option[String]) => String): Command =
    Command.args(name, helpMsg) { (state: State, args: Seq[String]) =>
      Runtime.default.unsafeRun(
        execGenCommand(helpMsg, args.toList, writer)
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
      |defined in the provided GraphQL schema defined at `schemaPath`. Instead of a path,
      |you can provide a URL and introspection will be used to gather the schema.
      |
      |The generated code will be formatted with Scalafmt using the configuration defined by
      |`scalafmtPath` (default: ".scalafmt.conf").
      |
      |""".stripMargin

  private val genClientHelpMsg =
    """
      |calibanGenClient schemaPath outputPath ?scalafmtPath
      |
      |This command will create a Scala file in `outputPath` containing client code for all the
      |typed defined in the provided GraphQL schema defined at `schemaPath`. Instead of a path,
      |you can provide a URL and introspection will be used to gather the schema.
      |
      |The generated code will be formatted with Scalafmt using the configuration defined by
      |`scalafmtPath` (default: ".scalafmt.conf").
      |
      |""".stripMargin

  def execGenCommand(
    helpMsg: String,
    args: List[String],
    writer: (Document, String, Option[String]) => String
  ): RIO[Console, Unit] =
    args match {
      case schemaPath :: toPath :: Nil               => Codegen.generate(schemaPath, toPath, None, writer)
      case schemaPath :: toPath :: formatPath :: Nil => Codegen.generate(schemaPath, toPath, Some(formatPath), writer)
      case _                                         => putStrLn(helpMsg)
    }

}
