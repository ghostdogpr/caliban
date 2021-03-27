package caliban.codegen

import caliban.tools.Codegen.GenType
import caliban.tools._
import sbt.Keys.commands
import sbt.{ AutoPlugin, Command, State }
import zio.console.{ putStrLn, Console }
import zio.{ RIO, Runtime }

object CodegenPlugin extends AutoPlugin {
  override lazy val projectSettings = Seq(commands ++= Seq(genSchemaCommand, genClientCommand))

  lazy val genSchemaCommand =
    genCommand(
      "calibanGenSchema",
      genSchemaHelpMsg,
      GenType.Schema
    )

  lazy val genClientCommand =
    genCommand(
      "calibanGenClient",
      genClientHelpMsg,
      GenType.Client
    )

  def genCommand(
    name: String,
    helpMsg: String,
    genType: GenType
  ): Command =
    Command.args(name, helpMsg) { (state: State, args: Seq[String]) =>
      Runtime.default.unsafeRun(
        execGenCommand(helpMsg, args.toList, genType)
          .catchAll(reason => putStrLn(reason.toString) *> putStrLn(reason.getStackTrace.mkString("\n")))
          .as(1)
      )
      state
    }

  private val commonHelp =
    """
      |The generated code will be formatted with Scalafmt using the configuration defined by
      |`--scalafmtPath` option (default: ".scalafmt.conf").
      |
      |If you provide a URL for `schemaPath`, you can provide request headers with
      |`--headers` option.
      |
      |The package of the generated code is derived from the folder of `outputPath`.
      |This can be overridden by providing an alternative package with the `--packageName`
      |option.
  """.stripMargin

  private val genSchemaHelpMsg =
    s"""
       |calibanGenSchema schemaPath outputPath [--scalafmtPath path] [--headers name:value,name2:value2] [--packageName name] [--effect fqdn.Effect]
       |
       |This command will create a Scala file in `outputPath` containing all the types
       |defined in the provided GraphQL schema defined at `schemaPath`. Instead of a path,
       |you can provide a URL and introspection will be used to gather the schema.
       |
       |$commonHelp
       |
       |By default, each Query and Mutation will be wrapped into a `zio.UIO` effect. 
       |This can be overridden by providing an alternative effect with the `--effect` option.
       |""".stripMargin

  private val genClientHelpMsg =
    s"""
       |calibanGenClient schemaPath outputPath [--scalafmtPath path] [--headers name:value,name2:value2] [--packageName name] [--genView true|false]
       |
       |This command will create a Scala file in `outputPath` containing client code for all the
       |typed defined in the provided GraphQL schema defined at `schemaPath`. Instead of a path,
       |you can provide a URL and introspection will be used to gather the schema.
       |
       |$commonHelp
       |""".stripMargin

  def execGenCommand(
    helpMsg: String,
    args: List[String],
    genType: GenType
  ): RIO[Console, Unit] =
    Options.fromArgs(args) match {
      case Some(arguments) =>
        for {
          _ <- putStrLn(s"Generating code for ${arguments.schemaPath}")
          _ <- Codegen.generate(arguments, genType)
          _ <- putStrLn(s"Code generation done")
        } yield ()
      case None            => putStrLn(helpMsg)
    }
}
