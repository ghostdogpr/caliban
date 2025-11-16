package caliban.codegen.compiletime

import caliban._
import caliban.codegen.Codegen.GenType
import caliban.codegen.compiletime.Config.ClientGenerationSettings
import caliban.codegen.Codegen
import caliban.tools.SchemaLoader
import zio.{ Task, ZIO }

object CompileTime {

  def generateClient[R](
    args: List[String]
  )(api: GraphQL[R], settings: ClientGenerationSettings): Task[Unit] =
    args match {
      case baseDir :: Nil =>
        Codegen
          .generate(
            SchemaLoader.fromCaliban(api),
            settings.toCalibanCommonSettings.toOptions(
              schemaPath = "",
              toPath = {
                val dir = Utils.toPathDir(baseDir, settings.packageName)

                if (settings.splitFiles) dir else s"$dir/${settings.clientName}.scala"
              }
            ),
            GenType.Client
          )
          .unit
      case _              =>
        ZIO
          .fail(new RuntimeException(s"`CompileTime.generateClient` was called with invalid arguments: $args"))
    }

}
