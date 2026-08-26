package caliban.gateway.benchmark

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, RemoteGraphQLConfig, Subgraph }
import sttp.model.{ Header => SttpHeader, Uri }
import zio._
import zio.http._

object Main extends ZIOAppDefault {

  private val DefaultSubgraphsUrl = "http://127.0.0.1:4200"
  private val DefaultPort         = 4000
  private val BenchmarkConfig     = RemoteGraphQLConfig.default.withExecution(_.withInFlightQueryDeduplication(true))

  override def run =
    program.tapErrorCause(cause => ZIO.logErrorCause("Gateway benchmark adapter failed.", cause))

  private val program =
    for {
      subgraphsUrl <- System.envOrElse("BENCHMARK_SUBGRAPHS_URL", DefaultSubgraphsUrl)
      portText     <- System.envOrElse("BENCHMARK_GATEWAY_PORT", DefaultPort.toString)
      uniqueText   <- System.envOrElse("BENCHMARK_UNIQUE_SOURCE_HEADERS", "false")
      port         <- ZIO
                        .attempt(portText.toInt)
                        .filterOrFail(port => port > 0 && port <= 65535)(
                          new IllegalArgumentException("BENCHMARK_GATEWAY_PORT must be between 1 and 65535.")
                        )
      unique       <- ZIO
                        .fromOption(uniqueText.toBooleanOption)
                        .orElseFail(
                          new IllegalArgumentException("BENCHMARK_UNIQUE_SOURCE_HEADERS must be true or false.")
                        )
      identities   <- Ref.make(0L)
      config        =
        if (unique)
          BenchmarkConfig.withExecutionHeadersZIO(
            identities
              .updateAndGet(_ + 1L)
              .map(value => List(SttpHeader("X-Caliban-Benchmark-Request-Id", value.toString)))
          )
        else BenchmarkConfig
      subgraphs    <- ZIO
                        .fromEither(benchmarkSubgraphs(subgraphsUrl, config))
                        .mapError(new IllegalArgumentException(_))
      interpreter  <- Gateway
                        .compose(subgraphs._1, subgraphs._2, subgraphs._3, subgraphs._4)
                        .interpreter
                        .mapError(error => new IllegalArgumentException(error.diagnostics.mkString(" ")))
      routes        = QuickAdapter(interpreter).routes("/graphql") ++ Routes(
                        Method.GET / "health" -> Handler.ok
                      )
      _            <- ZIO.logInfo(s"Serving the gateway benchmark on port $port.")
      _            <- Server.serve(routes).provide(Server.defaultWithPort(port))
    } yield ()

  private[benchmark] def benchmarkSubgraphs(
    baseUrl: String,
    config: RemoteGraphQLConfig[Any] = BenchmarkConfig
  ): Either[String, (Subgraph[Any], Subgraph[Any], Subgraph[Any], Subgraph[Any])] =
    for {
      endpoint <- Uri.parse(baseUrl.stripSuffix("/"))
      _        <- Either.cond(
                    endpoint.scheme.exists(value => value == "http" || value == "https") && endpoint.host.nonEmpty,
                    (),
                    "Benchmark subgraph base endpoint must be an absolute HTTP URL."
                  )
    } yield (
      Subgraph.federation("accounts", endpoint.addPath("accounts"), config),
      Subgraph.federation("inventory", endpoint.addPath("inventory"), config),
      Subgraph.federation("products", endpoint.addPath("products"), config),
      Subgraph.federation("reviews", endpoint.addPath("reviews"), config)
    )
}
