package caliban.gateway

import caliban.gateway.internal.{ RemoteGatewayRuntime, RemoteGraphQLSource }
import caliban.tools.RemoteSchema
import sttp.client4.httpclient.zio.HttpClientZioBackend
import zio._

/**
 * An immutable description of a gateway.
 *
 * A description is reusable: each call to [[build]] creates a new [[GatewayRuntime]] whose resources
 * are owned by the surrounding [[zio.Scope]]. `R` describes the environment required when executing
 * requests; constructing and building the description does not require that environment.
 */
final class Gateway[-R] private[gateway] (
  private val composer: Trace => ZIO[Scope, GatewayBuildError, GatewayRuntime[R]]
) {

  /**
   * Builds an executable runtime within the current scope.
   */
  def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    composer(trace)
}

object Gateway {

  /**
   * Creates a reusable gateway description from one or more subgraphs.
   */
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R] =
    new Gateway[R](trace => build(first, rest)(trace))

  private def build[R](first: Subgraph[R], rest: Seq[Subgraph[R]])(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    if (rest.nonEmpty)
      ZIO.fail(GatewayBuildError("This gateway version can execute exactly one subgraph."))
    else if (first.name.trim.isEmpty)
      ZIO.fail(GatewayBuildError("Subgraph name must not be empty."))
    else
      for {
        document <- first.schema.document.mapError(error => GatewayBuildError(error.getMessage))
        rootType <-
          ZIO.fromEither(RemoteSchema.toRootType(document)).mapError(error => GatewayBuildError(error.getMessage))
        backend  <- HttpClientZioBackend
                      .scoped()
                      .mapError(_ => GatewayBuildError("Unable to initialize the remote GraphQL transport."))
      } yield new RemoteGatewayRuntime[R](rootType, new RemoteGraphQLSource(first.endpoint, backend))
}
