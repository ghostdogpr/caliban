package caliban

import caliban.Configurator.ExecutionConfiguration
import zio._
import zio.http._
import zio.http.netty.NettyConfig
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.stacktracer.TracingImplicits.disableAutoTrace

final class QuickAdapter[R] private (requestHandler: QuickRequestHandler[R]) {

  private implicit val trace: Trace = Trace.empty

  /**
   * Converts this adapter to a [[QuickHandlers]] which contains [[zio.http.RequestHandler]]s for manually constructing zio-http routes
   */
  val handlers: QuickHandlers[R] = QuickHandlers(
    api = Handler.fromFunctionZIO[Request](requestHandler.handleHttpRequest),
    upload = Handler.fromFunctionZIO[Request](requestHandler.handleUploadRequest),
    webSocket = Handler.fromFunctionZIO[Request](requestHandler.handleWebSocketRequest)
  )

  @deprecated("Use `handlers` instead", "2.5.0")
  lazy val handler: RequestHandler[R, Nothing] =
    Handler.fromFunctionZIO[Request](requestHandler.handleHttpRequest)

  /**
   * Converts this adapter to an `Routes` serving the GraphQL API at the specified path.
   *
   * @param apiPath The path where the GraphQL API will be served.
   * @param graphiqlPath The path where the GraphiQL UI will be served. If None, GraphiQL will not be served.
   * @param uploadPath The path where files can be uploaded. If None, uploads will be disabled.
   * @param webSocketPath The path where websocket requests will be set. If None, websocket-based subscriptions will be disabled.
   */
  def routes(
    apiPath: String,
    graphiqlPath: Option[String] = None,
    uploadPath: Option[String] = None,
    webSocketPath: Option[String] = None
  ): Routes[R, Nothing] = {
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
    val wsRoute       = webSocketPath.toList.map { wsPath =>
      RoutePattern(Method.ANY, wsPath) -> handlers.webSocket
    }
    Routes.fromIterable(apiRoutes ::: graphiqlRoute ::: uploadRoute ::: wsRoute)
  }

  @deprecated("Use `routes` instead", "2.6.1")
  def toApp(
    apiPath: String,
    graphiqlPath: Option[String] = None,
    uploadPath: Option[String] = None,
    webSocketPath: Option[String] = None
  ): HttpApp[R] =
    HttpApp(routes(apiPath, graphiqlPath, uploadPath, webSocketPath))

  /**
   * Runs the server using the default zio-http server configuration on the specified port.
   * This is meant as a convenience method for getting started quickly
   *
   * @param port The port to serve the API on
   * @param apiPath The route to serve the API on, e.g., `/api/graphql`
   * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
   * @param uploadPath The route where files can be uploaded, e.g., /upload/graphql. If None, uploads will be disabled.
   * @param webSocketPath The path where websocket requests will be set. If None, websocket-based subscriptions will be disabled.
   */
  def runServer(
    port: Int,
    apiPath: String,
    graphiqlPath: Option[String] = None,
    uploadPath: Option[String] = None,
    webSocketPath: Option[String] = None
  )(implicit trace: Trace, tag: Tag[R]): RIO[R, Nothing] =
    Server
      .serve[R](routes(apiPath, graphiqlPath = graphiqlPath, uploadPath = uploadPath, webSocketPath = webSocketPath))
      .provideSomeLayer[R](
        ZLayer.succeed(Server.Config.default.port(port))
          ++ ZLayer.succeed(NettyConfig.default.leakDetection(LeakDetectionLevel.DISABLED))
          >+> Server.customized
      )

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickAdapter[R] =
    new QuickAdapter(requestHandler.configure(config))

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit trace: Trace): QuickAdapter[R & R1] =
    new QuickAdapter(requestHandler.configure[R1](configurator))

  def configureWebSocket[R1](config: quick.WebSocketConfig[R1]): QuickAdapter[R & R1] =
    new QuickAdapter(requestHandler.configureWebSocket(config))

}

object QuickAdapter {
  type Configurator[-R] = URIO[R & Scope, Unit]

  def apply[R](interpreter: GraphQLInterpreter[R, Any]): QuickAdapter[R] =
    new QuickAdapter(new QuickRequestHandler(interpreter, quick.WebSocketConfig.default))

  def handlers[R](implicit tag: Tag[R], trace: Trace): URIO[QuickAdapter[R], QuickHandlers[R]] =
    ZIO.serviceWith(_.handlers)

  def default[R](implicit
    tag: Tag[R],
    trace: Trace
  ): ZLayer[GraphQL[R], CalibanError.ValidationError, QuickAdapter[R]] = ZLayer.fromZIO(
    ZIO.serviceWithZIO(_.interpreter.map(QuickAdapter(_)))
  )

  def live[R](implicit
    tag: Tag[R],
    trace: Trace
  ): ZLayer[GraphQL[R] & ExecutionConfiguration, CalibanError.ValidationError, QuickAdapter[R]] =
    ZLayer.fromZIO(
      for {
        config      <- ZIO.service[ExecutionConfiguration]
        interpreter <- ZIO.serviceWithZIO[GraphQL[R]](_.interpreter)
      } yield QuickAdapter(interpreter).configure(config)
    )

}
