package caliban.uzhttp

import java.net.InetSocketAddress

import _root_.uzhttp.server._
import caliban.ExampleData._
import caliban._
import zio.{ App, ExitCode, ZEnv, ZIO }

object ExampleApp extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- ExampleApi.api.interpreter
      address     = new InetSocketAddress(8088)
      route       = UzHttpAdapter.makeHttpService("/api/graphql", interpreter)
      wsRoute     = UzHttpAdapter.makeWebSocketService("/ws/graphql", interpreter)
      server      = Server.builder(address).handleSome(route orElse wsRoute)
      _           <- server.serve.useForever.provideCustomLayer(ExampleService.make(sampleCharacters))
    } yield ()).exitCode

}
