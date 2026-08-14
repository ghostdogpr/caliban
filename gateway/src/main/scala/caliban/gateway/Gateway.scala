package caliban.gateway

import caliban.gateway.internal.{ RemoteGatewayRuntime, RemoteGraphQLSource, SchemaComposition, SchemaContribution }
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
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] = {
    val subgraphs = first +: rest.toList

    for {
      loaded       <- ZIO.foreach(subgraphs)(load(_).either)
      contributions = loaded.collect { case Right(contribution) => contribution }
      diagnostics   = nameDiagnostics(subgraphs) ::: loaded.collect { case Left(diagnostic) => diagnostic }
      composed      = SchemaComposition.compose(contributions)
      graph        <- ZIO
                        .fromEither(
                          composed.left.map(errors => GatewayBuildError((diagnostics ::: errors).distinct.sorted))
                        )
                        .flatMap { graph =>
                          if (diagnostics.isEmpty) ZIO.succeed(graph)
                          else ZIO.fail(GatewayBuildError(diagnostics.distinct.sorted))
                        }
      backend      <- HttpClientZioBackend
                        .scoped()
                        .mapError(_ => GatewayBuildError("Unable to initialize the remote GraphQL transport."))
      sources       =
        subgraphs.iterator.map(subgraph => subgraph.name -> new RemoteGraphQLSource(subgraph.endpoint, backend)).toMap
    } yield new RemoteGatewayRuntime[R](graph, sources)
  }

  private def load[R](subgraph: Subgraph[R])(implicit trace: Trace): IO[String, SchemaContribution] =
    for {
      document <- subgraph.schema.document.mapError(error => s"[${subgraph.name}] ${error.getMessage}")
      rootType <- ZIO
                    .fromEither(RemoteSchema.toRootType(document))
                    .mapError(error => s"[${subgraph.name}] ${error.getMessage}")
    } yield SchemaContribution(subgraph.name, rootType)

  private def nameDiagnostics[R](subgraphs: List[Subgraph[R]]): List[String] = {
    val blank     = subgraphs.collect {
      case subgraph if subgraph.name.trim.isEmpty => "[subgraph] Name must not be empty."
    }
    val duplicate = subgraphs
      .groupBy(_.name)
      .collect { case (name, values) if values.size > 1 => s"[subgraph '$name'] Name is used more than once." }
      .toList

    (blank ::: duplicate).sorted
  }
}
