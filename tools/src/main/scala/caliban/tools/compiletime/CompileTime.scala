package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.{ Codegen, SchemaLoader }
import zio.{ ExitCode, Task, URIO, ZEnv }

object CompileTime {

  def generateClient[R](args: List[String], api: GraphQL[R]): URIO[ZEnv, ExitCode] =
    args match {
      case toPath :: calibanSettings =>
        (
          for {
            _      <- Task.unit
            options = CompileTimeUtils.calibanCommonSettingsEquivalence
                        .from(calibanSettings)
                        .toOptions(schemaPath = "", toPath = toPath)
            _      <- Codegen
                        .generate(
                          SchemaLoader.fromCaliban(api),
                          options,
                          GenType.Client
                        )
          } yield ()
        ).exitCode
      case _                         => URIO.unit.exitCode
    }

}
