package caliban

import caliban.Configurator.ExecutionConfiguration
import zio._
import zio.http._

final class QuickAdapter[-R, E] private (requestHandler: QuickRequestHandler[R, E]) {

  /**
   * Converts this adapter to a [[Handler]] which can be used to create zio-http [[App]]s
   */
  val handler: RequestHandler[R, Response] =
    Handler.fromFunctionZIO[Request](requestHandler.handleRequest)

  /**
   * Converts this adapter to an [[App]] serving the GraphQL API at the specified path.
   *
   * @param apiPath The path where the GraphQL API will be served.
   * @param graphiqlPath The path where the GraphiQL UI will be served. If None, GraphiQL will not be served.
   */
  def toApp(apiPath: Path, graphiqlPath: Option[Path] = None): App[R] = {
    val apiApp = Http.collectHandler[Request] { case _ -> path if path == apiPath => handler }
    graphiqlPath match {
      case None         => apiApp
      case Some(uiPath) =>
        val uiHandler = GraphiQLAdapter.handler(apiPath, uiPath)
        apiApp ++ Http.collectHandler[Request] { case _ -> path if path == uiPath => uiHandler }
    }
  }

  /**
   * Runs the server using the default zio-http server configuration on the specified port.
   * This is meant as a convenience method for getting started quickly
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
