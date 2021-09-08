package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.{ CalibanCommonSettings, Codegen, SchemaLoader }
import zio.blocking.effectBlocking
import zio.{ ExitCode, URIO, ZEnv }

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object CompileTime {
  import Utils._

  final case class GenerateClientsSettings(
    clientName: String,
    packageName: String,
    scalafmtPath: Option[String] = None,
    headers: List[(String, String)] = List.empty,
    genView: Boolean = false,
    scalarMappings: List[(String, String)] = List.empty,
    imports: List[String] = List.empty,
    splitFiles: Boolean = false,
    enableFmt: Boolean = true,
    extensibleEnums: Boolean = false
  )                              {
    def toCalibanCommonSettings: CalibanCommonSettings =
      CalibanCommonSettings(
        clientName = Some(clientName),
        scalafmtPath = scalafmtPath,
        headers = headers,
        packageName = Some(packageName),
        genView = Some(genView),
        scalarMappings = scalarMappings,
        imports = imports,
        splitFiles = Some(splitFiles),
        enableFmt = Some(enableFmt),
        extensibleEnums = Some(extensibleEnums)
      )
  }
  object GenerateClientsSettings {
    def default: GenerateClientsSettings = GenerateClientsSettings(clientName = "Client", packageName = "generated")
  }

  def generateClient[R](args: List[String])(settings: GenerateClientsSettings, api: GraphQL[R]): URIO[ZEnv, ExitCode] =
    args match {
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
    }

}
