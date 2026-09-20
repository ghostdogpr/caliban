package caliban.gateway

import caliban.GraphQL
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.composition.RemoteSchemaAcquisition
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
  import Subgraph.{ Executable, Source }

  /**
   * Adds an explicit ordinary GraphQL object lookup to this subgraph.
   */
  def withLookup(lookup: Lookup): Subgraph[R] =
    new Subgraph[R](name, source, lookup :: lookups, transformations)

  /**
   * Applies structural schema transformations to this subgraph before composition.
   */
  def transform(values: SchemaTransformation*): Subgraph[R] =
    new Subgraph[R](name, source, lookups, transformations ::: values.toList)

  private[gateway] def load[R1 <: R](
    http: GatewayHttpClient,
    remoteErrorMessages: Boolean,
    hooks: PhaseHooks[R1]
  )(implicit trace: Trace): ZIO[Scope, SubgraphBuildError, Executable[R1]] =
    source match {
      case remote @ Source.Remote(endpoint, _, _, config) =>
        for {
          _        <- remote.validateConfig
          document <- RemoteSchemaAcquisition.load(remote, http)
          executor <- RemoteSubgraphExecutor.make(name, endpoint, http, config, hooks, remoteErrorMessages)
        } yield Executable(this, document, executor)
      case Source.Local(graph, _)                         =>
        ZIO
          .fromEither(graph.interpreterEither)
          .mapBoth(
            SubgraphBuildError.SchemaValidationFailed(_),
            interpreter => Executable(this, graph.toDocument, new LocalSubgraphExecutor(interpreter))
          )
    }
}

object Subgraph {

  /**
   * Describes an ordinary remote GraphQL graph whose schema is acquired through introspection.
   */
  def graphql(name: String, endpoint: URL): Subgraph[Any] =
    graphql(name, endpoint, RemoteGraphQLConfig.default)

  /**
   * Describes an ordinary remote GraphQL graph with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: URL, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Acquired, federation = false, config = config)

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL.
   */
  def graphql(name: String, endpoint: URL, schema: String): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: URL, schema: String, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Sdl(schema), federation = false, config = config)

  /**
   * Describes an ordinary remote GraphQL graph from an already parsed schema document.
   */
  def graphql(name: String, endpoint: URL, schema: Document): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes an ordinary remote GraphQL graph from a parsed document with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: URL, schema: Document, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Parsed(schema), federation = false, config = config)

  /**
   * Describes an ordinary in-process Caliban graph whose environment is supplied when the gateway executes.
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
    remote(name, endpoint, SchemaInput.Sdl(schema), federation = true, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from an already parsed schema document.
   */
  def federation(name: String, endpoint: URL, schema: Document): Subgraph[Any] =
    federation(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph from a parsed document with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: URL, schema: Document, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Parsed(schema), federation = true, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph whose schema is acquired through `_service`.
   */
  def federation(name: String, endpoint: URL): Subgraph[Any] =
    federation(name, endpoint, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: URL, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Acquired, federation = true, config = config)

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

  private[gateway] final case class Executable[-R](
    subgraph: Subgraph[R],
    document: Document,
    executor: SubgraphExecutor[R]
  )

  private[gateway] sealed trait Source[-R] {
    def federation: Boolean
  }

  private[gateway] object Source {
    final case class Remote[R](endpoint: URL, schema: SchemaInput, federation: Boolean, config: RemoteGraphQLConfig[R])
        extends Source[R] {
      def validateConfig(implicit trace: Trace): IO[SubgraphBuildError, Unit] = {
        val diagnostics = config.diagnostics(includeAcquisition = schema == SchemaInput.Acquired)
        ZIO.fail(SubgraphBuildError.InvalidConfiguration(diagnostics)).when(diagnostics.nonEmpty).unit
      }
    }
    final case class Local[R](graph: GraphQL[R], federation: Boolean) extends Source[R]
  }

  private[gateway] sealed trait SchemaInput

  private[gateway] object SchemaInput {
    final case class Sdl(value: String)      extends SchemaInput
    final case class Parsed(value: Document) extends SchemaInput
    case object Acquired                     extends SchemaInput
  }
}
