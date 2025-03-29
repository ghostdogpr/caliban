package example.calibantotapir

import example.calibantotapir.graphql._
import zio._
import zio.http._
import caliban._
import caliban.interop.tapir._
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.ztapir.RIOMonadError
import sttp.tapir.server.ziohttp._
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import example.calibantotapir.tapir.SampleRestEndpoint
import sttp.monad.MonadError

object MainApp extends ZIOAppDefault {
  implicit val monadErrorForTask: MonadError[Task] = new RIOMonadError[Any]

  /**
   * Creates a list of endpoints:
   *   - GraphiQL UI (GET /graphiql)
   *   - GraphQL API (POST & GET /graphql)
   *   - Example endpoint (GET /example)
   */
  val graphQLToTapir: ZIO[Queries & Mutations, CalibanError, List[ServerEndpoint[ZioStreams, Task]]] =
    for {
      queries         <- ZIO.service[Queries]
      mutations       <- ZIO.service[Mutations]
      graphql          = graphQL(RootResolver(queries, mutations))
      interpreter     <- graphql.interpreter
      graphqlApiPath   = "graphql"
      // NOTE: We mount the GraphQL endpoints (POST and GET) on /graphql
      graphqlEndpoints = HttpInterpreter(interpreter)
                           .serverEndpoints[Any, ZioStreams](ZioStreams)
                           .map(_.prependSecurityIn(graphqlApiPath))
      // NOTE: We make GraphiQL point to the api path hosted on /graphql and we mount the GraphiQL UI on /graphiql
      graphiqlEndpoint = HttpInterpreter
                           .makeGraphiqlEndpoint[Task](apiPath = graphqlApiPath)
                           .prependSecurityIn("graphiql")
      endpoints        = graphiqlEndpoint :: graphqlEndpoints
    } yield endpoints

  // Produces a Swagger UI for the endpoints on GET /docs
  def documented(allEndpoints: List[ServerEndpoint[ZioStreams, Task]]) = {
    val doc = SwaggerInterpreter().fromServerEndpoints(allEndpoints, "Caliban tapir playground", "1.0")
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
