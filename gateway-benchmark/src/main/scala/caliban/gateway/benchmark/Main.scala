package caliban.gateway.benchmark

import caliban.QuickAdapter
import caliban.gateway.{ traverseEither, Gateway, RemoteGraphQLConfig, Subgraph }
import zio._
import zio.http._

object Main extends ZIOAppDefault {

  private val DefaultSubgraphsHost = "127.0.0.1"
  private val DefaultPort          = 5220
  private val SubgraphPorts        =
    List("accounts" -> 5221, "inventory" -> 5222, "products" -> 5223, "reviews" -> 5224)

  override def run: RIO[Scope, Unit] =
    program.tapErrorCause(cause => ZIO.logErrorCause("Gateway benchmark adapter failed.", cause))

  private val program =
    for {
      host        <- System.envOrElse("BENCHMARK_SUBGRAPHS_HOST", DefaultSubgraphsHost)
      portText    <- System.envOrElse("BENCHMARK_GATEWAY_PORT", DefaultPort.toString)
      port        <- ZIO
                       .attempt(portText.toInt)
                       .filterOrFail(port => port > 0 && port <= 65535)(
                         new IllegalArgumentException("BENCHMARK_GATEWAY_PORT must be between 1 and 65535.")
                       )
      subgraphs   <- ZIO.fromEither(benchmarkSubgraphs(host)).mapError(new IllegalArgumentException(_))
      interpreter <- Gateway
                       .compose(subgraphs.head, subgraphs.tail: _*)
                       .interpreter
                       .mapError(error => new IllegalArgumentException(error.diagnostics.mkString(" ")))
      routes       = QuickAdapter(interpreter).routes("/graphql") ++ Routes(Method.GET / "health" -> Handler.ok)
      _           <- ZIO.logInfo(s"Serving the gateway benchmark on port $port.")
      _           <- Server.serve(routes).provide(Server.defaultWithPort(port))
    } yield ()

  private[benchmark] def benchmarkSubgraphs(host: String): Either[String, List[Subgraph[Any]]] = {
    val config = RemoteGraphQLConfig.default.withExecution(_.forwardIncomingHeaders("Authorization"))
    if (host.isEmpty || host.exists(char => char.isWhitespace || char == '/'))
      Left("BENCHMARK_SUBGRAPHS_HOST must be a host name or address.")
    else {
      traverseEither(SubgraphPorts) { case (name, port) =>
        URL
          .decode(s"http://$host:$port/graphql")
          .left
          .map(_.getMessage)
          .map(url => Subgraph.federation(name, url, config))
      }
    }
  }
}
