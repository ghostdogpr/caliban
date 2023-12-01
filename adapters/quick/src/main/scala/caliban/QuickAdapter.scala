package caliban

import caliban.Configurator.ExecutionConfiguration
import zio._
import zio.http._

final class QuickAdapter[-R, E] private (requestHandler: QuickRequestHandler[R, E]) {

  /**
   * Converts this adapter to a [[zio.http.RequestHandler]] which can be used to create zio-http [[zio.http.App]]s
   */
  val handler: RequestHandler[R, Nothing] =
    Handler.fromFunctionZIO[Request](requestHandler.handleRequest)

  /**
   * Converts this adapter to an [[zio.http.App]] serving the GraphQL API at the specified path.
   *
   * @param apiPath The path where the GraphQL API will be served.
   * @param graphiqlPath The path where the GraphiQL UI will be served. If None, GraphiQL will not be served.
   */
  def toApp(apiPath: Path, graphiqlPath: Option[Path] = None): App[R] = {
    val apiApp = Http.collectHandler[Request] {
      case (Method.GET | Method.POST) -> path if path == apiPath => handler
    }
    graphiqlPath match {
      case None         => apiApp
      case Some(uiPath) =>
        val uiHandler = GraphiQLHandler.handler(apiPath.toString(), uiPath.toString)
        apiApp ++ Http.collectHandler[Request] {
          case Method.GET -> path if path == uiPath => uiHandler
        }
    }
  }

  /**
   * Runs the server using the default zio-http server configuration on the specified port.
   * This is meant as a convenience method for getting started quickly
   *
   * @param port The port to serve the API on
   * @param apiPath The route to serve the API on, e.g., `/api/graphql`
   * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
   */
  def runServer(port: Int, apiPath: String, graphiqlPath: Option[String] = None)(implicit
    trace: Trace
  ): RIO[R, Nothing] =
    Server
      .serve[R](toApp(Path.decode(apiPath), graphiqlPath.map(Path.decode)))
      .provideSomeLayer[R](Server.defaultWithPort(port))

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickAdapter[R, E] =
    new QuickAdapter(requestHandler.configure(config))

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit trace: Trace): QuickAdapter[R & R1, E] =
    new QuickAdapter(requestHandler.configure[R1](configurator))

}

object QuickAdapter {
  type Configurator[-R] = URIO[R & Scope, Unit]

  def apply[R, E](interpreter: GraphQLInterpreter[R, E]): QuickAdapter[R, E] =
    new QuickAdapter(new QuickRequestHandler(interpreter))

  def handler[R](implicit tag: Tag[R], trace: Trace): URIO[QuickAdapter[R, CalibanError], RequestHandler[R, Response]] =
    ZIO.serviceWith(_.handler)

  def default[R](implicit
    tag: Tag[R],
    trace: Trace
  ): ZLayer[GraphQL[R], CalibanError.ValidationError, QuickAdapter[R, CalibanError]] = ZLayer.fromZIO(
    ZIO.serviceWithZIO(_.interpreter.map(QuickAdapter(_)))
  )

  def live[R](implicit
    tag: Tag[R],
    trace: Trace
  ): ZLayer[GraphQL[R] & ExecutionConfiguration, CalibanError.ValidationError, QuickAdapter[R, CalibanError]] =
    ZLayer.fromZIO(
      for {
        config      <- ZIO.service[ExecutionConfiguration]
        interpreter <- ZIO.serviceWithZIO[GraphQL[R]](_.interpreter)
      } yield QuickAdapter(interpreter).configure(config)
    )

}
