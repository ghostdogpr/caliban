package caliban

import caliban.Configurator.ExecutionConfiguration
import zio.http.{ HttpApp, Path, RequestHandler, Response }
import zio.{ RIO, Trace, ZIO }

package object quick {

  implicit class GraphqlServerOps[R](val gql: GraphQL[R]) extends AnyVal {

    /**
     * Serves the GraphQL API on the specified port using the default zio-http configuration.
     * This is meant as a convenience method for getting started quickly.
     *
     * @param port The port to serve the API on
     * @param apiPath The route to serve the API on, e.g., `/api/graphql`
     * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
     */
    def runServer(port: Int, apiPath: String, graphiqlPath: Option[String] = None)(implicit
      trace: Trace
    ): RIO[R, Nothing] =
      gql.interpreter.flatMap(QuickAdapter(_).runServer(port, apiPath, graphiqlPath))

    /**
     * Creates zio-http [[zio.http.HttpApp]] from the GraphQL API
     *
     * @param apiPath The route to serve the API on, e.g., `/api/graphql`
     * @param graphiqlPath Optionally define a route to serve the GraphiQL UI on, e.g., `/graphiql`
     */
    def toApp(apiPath: String, graphiqlPath: Option[String] = None)(implicit
      trace: Trace
    ): RIO[R, HttpApp[R]] =
      gql.interpreter.map(QuickAdapter(_).toApp(Path.decode(apiPath), graphiqlPath.map(Path.decode)))

    /**
     * Creates a zio-http handler for the GraphQL API
     *
     * @see [[handlerConfigured]] for a more powerful variant that allows configuring the GraphQL request execution
     */
    def handler(implicit trace: Trace): ZIO[R, CalibanError.ValidationError, RequestHandler[R, Response]] =
      gql.interpreter.map(QuickAdapter(_).handler)

    /**
     * Creates a zio-http handler for the GraphQL API, allowing to configure the GraphQL request execution
     */
    def handlerConfigured(config: ExecutionConfiguration)(implicit
      trace: Trace
    ): ZIO[R, CalibanError.ValidationError, RequestHandler[R, Response]] =
      gql.interpreter.map(QuickAdapter(_).configure(config).handler)
  }

}
