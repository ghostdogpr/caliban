package generator

import caliban.tools.compiletime.CompileTime
import caliban.tools.compiletime.CompileTime.GenerateClientsSettings
import io.guizmaii.poc.caliban.server.GraphQLApi
import zio.{ExitCode, URIO}

private[generator] object CalibanClientGenerator extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    CompileTime.generateClient(args)(GenerateClientsSettings.default, GraphQLApi.api)
  }
}
