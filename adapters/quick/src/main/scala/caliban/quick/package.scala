package caliban

import caliban.Configurator.ExecutionConfiguration
import zio.http._
import zio.{ RIO, Trace, ZIO, ZIOAppDefault }

package object quick {

  implicit class GraphqlServerOps[R](val gql: GraphQL[R]) extends AnyVal {

    /**
     * Serves the GraphQL API on the specified port using the default zio-http configuration.
     * This is meant as a convenience method for getting started quickly.
     *
     * @param port The port to serve the API on
     * @param apiPath The route to serve the API on, e.g., `/api/graphql`
     * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
     * @param uploadPath Optionally define a route to serve file uploads on, e.g., `/api/upload`
     */
    def runServer(
      port: Int,
      apiPath: String,
      graphiqlPath: Option[String] = None,
      uploadPath: Option[String] = None
    )(implicit
      trace: Trace
    ): RIO[R, Nothing] =
      gql.interpreter.flatMap(QuickAdapter(_).runServer(port, apiPath, graphiqlPath, uploadPath))

    /**
     * Creates zio-http `HttpApp` from the GraphQL API
     *
     * @param apiPath The route to serve the API on, e.g., `/api/graphql`
     * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
     * @param uploadPath Optionally define a route to serve file uploads on, e.g., `/api/upload`
     */
    def toApp(
      apiPath: String,
      graphiqlPath: Option[String] = None,
      uploadPath: Option[String] = None
    )(implicit
      trace: Trace
    ): RIO[R, HttpApp[R]] =
      gql.interpreter.map(QuickAdapter(_).toApp(apiPath, graphiqlPath, uploadPath))

    /**
     * Creates a zio-http handler for the GraphQL API
     *
     * @see [[handlersConfigured]] for a more powerful variant that allows configuring the GraphQL request execution
     */
    def handlers(implicit trace: Trace): ZIO[R, CalibanError.ValidationError, QuickHandlers[R]] =
      gql.interpreter.map(QuickAdapter(_).handlers)

    @deprecated("Use handlers instead", "2.5.0")
    def handler(implicit trace: Trace): ZIO[R, CalibanError.ValidationError, RequestHandler[R, Nothing]] =
      gql.interpreter.map(QuickAdapter(_).handler)

    /**
     * Creates a zio-http handler for the GraphQL API, allowing to configure the GraphQL request execution
     */
    def handlersConfigured(config: ExecutionConfiguration)(implicit
      trace: Trace
    ): ZIO[R, CalibanError.ValidationError, QuickHandlers[R]] =
      gql.interpreter.map(QuickAdapter(_).configure(config).handlers)

    @deprecated("Use handlersConfigured instead", "2.5.0")
    def handlerConfigured(config: ExecutionConfiguration)(implicit
      trace: Trace
    ): ZIO[R, CalibanError.ValidationError, RequestHandler[R, Response]] =
      gql.interpreter.map(QuickAdapter(_).configure(config).handler)
  }

  implicit class GraphqlAnyServerOps[R](val gql: GraphQL[Any]) extends AnyVal {

    /**
     * Convenience method for impurely running the server.
     *
     * Useful for scripts / demos / showing off Caliban to your colleagues etc.
     */
    def unsafeRunServer(
      port: Int = 8080,
      apiPath: String = "/graphql",
      graphiqlPath: Option[String] = Some("/graphiql"),
      uploadPath: Option[String] = None
    )(implicit trace: Trace): Unit =
      ZIOAppDefault(gql.runServer(port, apiPath, graphiqlPath, uploadPath)).main(Array.empty)
  }

}
