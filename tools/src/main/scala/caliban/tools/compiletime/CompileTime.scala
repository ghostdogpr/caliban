package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.compiletime.Config.GenerateClientSettings
import caliban.tools.{ Codegen, SchemaLoader }
import zio.{ ExitCode, Task, URIO, ZEnv }

object CompileTime {

  def generateClient[R](args: List[String])(api: GraphQL[R]): URIO[ZEnv, ExitCode] =
    args match {
      case baseDir :: rawSettings =>
        (
          for {
            _       <- Task.unit
            settings = GenerateClientSettings.fromArgs(rawSettings)
            _       <- Codegen
                         .generate(
                           SchemaLoader.fromCaliban(api),
                           settings.toCalibanCommonSettings.toOptions(
                             schemaPath = "",
                             toPath = Utils.toPath(baseDir, settings)
                           ),
                           GenType.Client
                         )
          } yield ()
        ).exitCode
      case _                      => URIO.unit.exitCode
    }

}
