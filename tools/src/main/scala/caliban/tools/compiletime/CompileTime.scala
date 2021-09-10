package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.compiletime.Config.GenerateClientSettings
import caliban.tools.{ Codegen, SchemaLoader }
import zio.{ ExitCode, URIO, ZEnv }

object CompileTime {

  def generateClient[R](args: List[String])(api: GraphQL[R], settings: GenerateClientSettings): URIO[ZEnv, ExitCode] =
    args match {
      case baseDir :: Nil =>
        Codegen
          .generate(
            SchemaLoader.fromCaliban(api),
            settings.toCalibanCommonSettings.toOptions(
              schemaPath = "",
              toPath = Utils.toPath(baseDir, settings.packageName, settings.clientName)
            ),
            GenType.Client
          )
          .exitCode
      case _              => URIO.unit.exitCode
    }

}
