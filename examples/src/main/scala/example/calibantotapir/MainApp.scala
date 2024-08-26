package example.calibantotapir

import example.calibantotapir.graphql._
import zio._
import zio.http._
import caliban._
import caliban.interop.tapir._
import sttp.tapir.json.jsoniter._
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.server.ziohttp._
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import example.calibantotapir.tapir.SampleRestEndpoint

object MainApp extends ZIOAppDefault {
  val graphQLToTapir: ZIO[Queries & Mutations, CalibanError, List[ServerEndpoint[ZioStreams, Task]]] =
    for {
      queries     <- ZIO.service[Queries]
      mutations   <- ZIO.service[Mutations]
      graphql      = graphQL(RootResolver(queries, mutations))
      interpreter <- graphql.interpreter
      endpoints    =
        HttpInterpreter(interpreter)
          .serverEndpoints[Any, ZioStreams](ZioStreams)
          .map(_.prependSecurityIn("graphql"))
    } yield endpoints

  def documented(allEndpoints: List[ServerEndpoint[ZioStreams, Task]]) = {
    val doc = SwaggerInterpreter().fromServerEndpoints(allEndpoints, "playground", "1.0")
    doc ++ allEndpoints
  }

  // Redirect / to /docs
  val redirectRootToDocs =
    Routes(
      Method.GET / "" ->
        handler(Response.redirect(URL(path = Path.root / "docs"), isPermanent = true))
    )

  val serve: ZIO[Queries & Mutations & Server, CalibanError, Nothing] =
    for {
      graphqlEndpoints <- graphQLToTapir
      restEndpoint      = SampleRestEndpoint.endpoint
      all               = documented(restEndpoint :: graphqlEndpoints)
      routes            = ZioHttpInterpreter().toHttp[Any](all)
      server           <- ZIO.service[Server]
      _                <- Server.install(redirectRootToDocs ++ routes)
      port             <- server.port
      _                <- ZIO.logInfo(s"Server started on port $port")
      result           <- ZIO.never
    } yield result

  override val run =
    serve
      .provide(
        BookRepository.layer,
        Queries.layer,
        Mutations.layer,
        Server.live,
        ZLayer.succeed(Server.Config.default)
      )
}
