package caliban.gateway

import caliban.GraphQL
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.acquisition.RemoteSchemaAcquisition
import caliban.gateway.internal.acquisition.RemoteSchemaAcquisition.parseSdl
import caliban.gateway.internal.composition.{ ComposedGraph, SchemaComposer }
import caliban.gateway.internal.execution.{ LocalSubgraphExecutor, RemoteSubgraphExecutor, SubgraphExecutor }
import caliban.parsing.adt.Document
import zio.{ IO, Scope, Trace, ZIO }
import zio.http.URL

/**
 * A named GraphQL graph that participates in gateway composition and execution.
 */
final class Subgraph[-R] private[gateway] (
  private[gateway] val name: String,
  private[gateway] val source: Subgraph.Source[R],
  private[gateway] val lookups: List[Lookup],
  private[gateway] val transformations: List[SchemaTransformation]
) {
  import Subgraph.{ Resolved, SchemaInput, Source }

  /**
   * Adds a lookup declared in Scala to this subgraph.
   */
  def withLookup(lookup: Lookup): Subgraph[R] =
    new Subgraph[R](name, source, lookup :: lookups, transformations)

  /**
   * Applies structural schema transformations to this subgraph before composition.
   */
  def transform(values: SchemaTransformation*): Subgraph[R] =
    new Subgraph[R](name, source, lookups, transformations ::: values.toList)

  private[gateway] def acquired: Boolean =
    source match {
      case Source.Remote(_, SchemaInput.Acquired(_) | SchemaInput.Fetched(_, _), _, _) => true
      case _                                                                           => false
    }

  /**
   * Introspection does not expose applied directives.
   */
  private[gateway] def introspected: Boolean =
    source match {
      case Source.Remote(_, SchemaInput.Acquired(_), federation, _) => !federation
      case _                                                        => false
    }

  private[gateway] def resolve(http: GatewayHttpClient)(implicit trace: Trace): IO[SubgraphBuildError, Resolved[R]] =
    (source match {
      case remote @ Source.Remote(_, _, _, _) => RemoteSchemaAcquisition.load(remote, http)
      case Source.Local(graph, _)             => ZIO.succeed(graph.toDocument)
    }).map(Resolved(this, _))

  private[gateway] def diagnostics: List[String] =
    (source match {
      case Source.Remote(endpoint, schema, _, config) =>
        val acquisition = schema match {
          case SchemaInput.Acquired(acquisition)     => acquisition.diagnostics
          case SchemaInput.Fetched(url, acquisition) =>
            checkAbsolute(url, _.isHttp, "Schema URL must be an absolute http or https URL.") :::
              acquisition.diagnostics
          case _                                     => Nil
        }
        // "foo" decodes into a relative URL, which would only fail later at request time.
        checkAbsolute(endpoint, _.isHttp, "Endpoint must be an absolute http or https URL.") :::
          config.execution.diagnostics ::: acquisition ::: config.subscription.diagnostics
      case _                                          => Nil
    }).map(message => s"[$name] $message")
}

object Subgraph {

  /**
   * Describes a remote GraphQL service whose schema is acquired through introspection, which does not expose applied
   * directives.
   */
  def graphql(name: String, endpoint: URL): Subgraph[Any] =
    graphql(name, endpoint, RemoteGraphQLConfig.default)

  /**
   * Describes a remote GraphQL service whose schema is acquired through introspection, with remote GraphQL and
   * schema-acquisition configuration.
   */
  def graphql[R](
    name: String,
    endpoint: URL,
    config: RemoteGraphQLConfig[R],
    acquisition: RemoteGraphQLConfig.Acquisition = RemoteGraphQLConfig.Acquisition.default
  ): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Acquired(acquisition), federation = false, config = config)

  /**
   * Describes a remote GraphQL service from pinned SDL.
   */
  def graphql(name: String, endpoint: URL, schema: String): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a remote GraphQL service from pinned SDL with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: URL, schema: String, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Pinned(parseSdl(schema)), federation = false, config = config)

  /**
   * Describes a remote GraphQL service from an already parsed schema document.
   */
  def graphql(name: String, endpoint: URL, schema: Document): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a remote GraphQL service from a parsed document with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: URL, schema: Document, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Pinned(Right(schema)), federation = false, config = config)

  /**
   * Describes a remote GraphQL service whose SDL is fetched with an HTTP GET from `schema`.
   */
  def graphql(name: String, endpoint: URL, schema: URL): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default, RemoteGraphQLConfig.Acquisition.default)

  /**
   * Describes a remote GraphQL service whose SDL is fetched with an HTTP GET from `schema`, with remote GraphQL
   * and schema-acquisition configuration.
   */
  def graphql[R](
    name: String,
    endpoint: URL,
    schema: URL,
    config: RemoteGraphQLConfig[R],
    acquisition: RemoteGraphQLConfig.Acquisition
  ): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Fetched(schema, acquisition), federation = false, config = config)

  /**
   * Describes an in-process Caliban GraphQL API whose environment is supplied when the gateway executes.
   */
  def graphql[R](name: String, graph: GraphQL[R]): Subgraph[R] =
    new Subgraph[R](name, Source.Local(graph, federation = false), Nil, Nil)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from pinned SDL.
   */
  def federation(name: String, endpoint: URL, schema: String): Subgraph[Any] =
    federation(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph from pinned SDL with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: URL, schema: String, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Pinned(parseSdl(schema)), federation = true, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from an already parsed schema document.
   */
  def federation(name: String, endpoint: URL, schema: Document): Subgraph[Any] =
    federation(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph from a parsed document with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: URL, schema: Document, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Pinned(Right(schema)), federation = true, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph whose schema is acquired through `_service`.
   */
  def federation(name: String, endpoint: URL): Subgraph[Any] =
    federation(name, endpoint, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph with remote GraphQL and schema-acquisition configuration.
   */
  def federation[R](
    name: String,
    endpoint: URL,
    config: RemoteGraphQLConfig[R],
    acquisition: RemoteGraphQLConfig.Acquisition = RemoteGraphQLConfig.Acquisition.default
  ): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Acquired(acquisition), federation = true, config = config)

  /**
   * Describes an in-process Federation graph whose environment is supplied when the gateway executes.
   * The graph must already provide its Federation entity resolvers.
   */
  def federation[R](name: String, graph: GraphQL[R]): Subgraph[R] =
    new Subgraph[R](name, Source.Local(graph, federation = true), Nil, Nil)

  private def remote[R](
    name: String,
    endpoint: URL,
    schema: SchemaInput,
    federation: Boolean,
    config: RemoteGraphQLConfig[R]
  ): Subgraph[R] =
    new Subgraph[R](name, Source.Remote(endpoint, schema, federation, config), Nil, Nil)

  private[gateway] final case class Resolved[-R](subgraph: Subgraph[R], document: Document) {

    def load[R1 <: R](http: GatewayHttpClient, remoteErrorMessages: Boolean, hooks: PhaseHooks[R1])(implicit
      trace: Trace
    ): ZIO[Scope, SubgraphBuildError, Executable[R1]] =
      (subgraph.source match {
        case Source.Remote(endpoint, _, _, config) =>
          RemoteSubgraphExecutor.make(subgraph.name, endpoint, http, config, hooks, remoteErrorMessages)
        case Source.Local(graph, _)                =>
          ZIO
            .fromEither(graph.interpreterEither)
            .mapBoth(SubgraphBuildError.SchemaValidationFailed(_), new LocalSubgraphExecutor(subgraph.name, _, hooks))
      }).flatMap(executor => ZIO.fromEither(SchemaComposer.prepare(subgraph, document)).map(Executable(_, executor)))
  }

  private[gateway] final case class Executable[-R](
    prepared: ComposedGraph.Source,
    executor: SubgraphExecutor[R]
  )

  private[gateway] sealed trait Source[-R] {
    def federation: Boolean
  }

  private[gateway] object Source {
    final case class Remote[R](endpoint: URL, schema: SchemaInput, federation: Boolean, config: RemoteGraphQLConfig[R])
        extends Source[R]
    final case class Local[R](graph: GraphQL[R], federation: Boolean) extends Source[R]
  }

  private[gateway] sealed trait SchemaInput

  private[gateway] object SchemaInput {
    final case class Pinned(document: Either[SchemaAcquisitionError, Document]) extends SchemaInput
    // Introspection, or `_service` for a Federation subgraph.
    final case class Acquired(config: RemoteGraphQLConfig.Acquisition)          extends SchemaInput
    final case class Fetched(url: URL, config: RemoteGraphQLConfig.Acquisition) extends SchemaInput
  }
}
