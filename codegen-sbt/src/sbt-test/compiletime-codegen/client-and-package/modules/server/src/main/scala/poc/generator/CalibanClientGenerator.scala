package poc.generator

import caliban.tools.compiletime.CompileTime
import caliban.tools.compiletime.CompileTime.GenerateClientsSettings
import io.guizmaii.poc.caliban.server.GraphQLApi
import zio.{ExitCode, URIO}

private[generator] object CalibanClientGenerator extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val settings =
      GenerateClientsSettings(
        clientName = "CalibanClient",
        packageName = "io.guizmaii.poc.caliban.client.generated"
      )

    CompileTime.generateClient(args)(settings, GraphQLApi.api)
  }
}
