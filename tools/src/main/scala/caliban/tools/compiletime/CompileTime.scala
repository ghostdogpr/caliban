package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.{ Codegen, Options, SchemaLoader }
import zio.{ ExitCode, Task, URIO, ZEnv }

object CompileTime {

  def generateClient[R](args: List[String], api: GraphQL[R]): URIO[ZEnv, ExitCode] =
    args match {
      case toPath :: packageName :: clientName :: _ =>
        (
          for {
            _      <- Task.unit
            // For now, we don't allow much Options customization. To improve later.
            options = Options(
                        schemaPath = "",
                        toPath = toPath,
                        fmtPath = None,
                        headers = None,
                        packageName = Some(packageName),
                        clientName = Some(clientName),
                        genView = None,
                        effect = None,
                        scalarMappings = None,
                        imports = None,
                        abstractEffectType = None,
                        splitFiles = None,
                        enableFmt = None,
                        extensibleEnums = None
                      )
            _      <- Codegen
                        .generate(
                          SchemaLoader.fromCaliban(api),
                          options,
                          GenType.Client
                        )
          } yield ()
        ).exitCode
      case _                                        => URIO.unit.exitCode
    }

}
