package caliban

import caliban.Configurator.ExecutionConfiguration
import zio._
import zio.http._

final class QuickAdapter[-R, E] private (requestHandler: QuickRequestHandler[R, E]) {

  /**
   * Converts this adapter to a [[QuickHandlers]] which contains [[zio.http.RequestHandler]]s for manually constructing zio-http routes
   */
  val handlers: QuickHandlers[R] = QuickHandlers(
    api = Handler.fromFunctionZIO[Request](requestHandler.handleHttpRequest),
    upload = Handler.fromFunctionZIO[Request](requestHandler.handleUploadRequest)
  )

  @deprecated("Use `handlers` instead", "2.5.0")
  lazy val handler: RequestHandler[R, Nothing] =
    Handler.fromFunctionZIO[Request](requestHandler.handleHttpRequest)

  /**
   * Converts this adapter to an `HttpApp` serving the GraphQL API at the specified path.
   *
   * @param apiPath The path where the GraphQL API will be served.
   * @param graphiqlPath The path where the GraphiQL UI will be served. If None, GraphiQL will not be served.
   * @param uploadPath The path where files can be uploaded. If None, uploads will be disabled.
   */
  def toApp(
    apiPath: String,
    graphiqlPath: Option[String] = None,
    uploadPath: Option[String] = None
  ): HttpApp[R] = {
    val apiRoutes     = List(
      RoutePattern(Method.POST, apiPath) -> handlers.api,
      RoutePattern(Method.GET, apiPath)  -> handlers.api
    )
    val graphiqlRoute = graphiqlPath.toList.map { uiPath =>
      RoutePattern(Method.GET, uiPath) -> GraphiQLHandler.handler(apiPath, uiPath)
    }
    val uploadRoute   = uploadPath.toList.map { uPath =>
      RoutePattern(Method.POST, uPath) -> handlers.upload
    }

    Routes.fromIterable(apiRoutes ::: graphiqlRoute ::: uploadRoute).toHttpApp
  }

  /**
   * Runs the server using the default zio-http server configuration on the specified port.
   * This is meant as a convenience method for getting started quickly
   *
   * @param port The port to serve the API on
   * @param apiPath The route to serve the API on, e.g., `/api/graphql`
   * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
   */
  def runServer(
    port: Int,
    apiPath: String,
    graphiqlPath: Option[String] = None,
    uploadPath: Option[String] = None
  )(implicit trace: Trace): RIO[R, Nothing] =
    Server
      .serve[R](toApp(apiPath, graphiqlPath = graphiqlPath, uploadPath = uploadPath))
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

  def handlers[R](implicit tag: Tag[R], trace: Trace): URIO[QuickAdapter[R, CalibanError], QuickHandlers[R]] =
    ZIO.serviceWith(_.handlers)

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
