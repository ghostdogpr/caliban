package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.{ Codegen, SchemaLoader }
import zio.{ ExitCode, URIO, ZEnv }

object CompileTime {

  def generateClient[R](args: List[String], api: GraphQL[R]): URIO[ZEnv, ExitCode] =
    args match {
      case toPath :: calibanSettings =>
        Codegen
          .generate(
            SchemaLoader.fromCaliban(api),
            CompileTimeUtils.calibanCommonSettingsEquivalence
              .from(calibanSettings)
              .toOptions(schemaPath = "", toPath = toPath),
            GenType.Client
          )
          .exitCode
      case _                         => URIO.unit.exitCode
    }

}
