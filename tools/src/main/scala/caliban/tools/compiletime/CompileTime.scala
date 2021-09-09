package caliban.tools.compiletime

import caliban.GraphQL
import zio.console._
import zio.{ ExitCode, URIO, ZEnv }

object CompileTime {

  def generateClient[R](args: List[String])(api: GraphQL[R]): URIO[ZEnv, ExitCode] =
    putStr(s"CALLED WITH: ${args.mkString(" ")}").exitCode

  /*args match {
      case baseDir :: _ =>
        (for {
          toPath <- effectBlocking {
                      val directory = s"$baseDir/src/main/scala/${packagePath(settings.packageName)}"

                      createDirectories(directory)

                      s"$directory/${settings.clientName}.scala"
                    }
          _      <- Codegen
                      .generate(
                        SchemaLoader.fromCaliban(api),
                        settings.toCalibanCommonSettings.toOptions(schemaPath = "", toPath = toPath),
                        GenType.Client
                      )
          _      <- effectBlocking {
                      val metadataDir = s"$baseDir/target/ctCaliban"

                      createDirectories(metadataDir)

                      Files.writeString(
                        new File(s"$metadataDir/metadata").toPath,
                        toPath,
                        StandardCharsets.UTF_8
                      )
                    }
        } yield ()).exitCode
      case _            => URIO.unit.exitCode
    }*/

}
