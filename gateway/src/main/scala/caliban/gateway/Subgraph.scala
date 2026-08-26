package caliban.gateway

import caliban.GraphQL
import caliban.parsing.adt.Document
import sttp.model.Uri

/**
 * A named GraphQL graph that participates in gateway composition and execution.
 */
final class Subgraph[-R] private[gateway] (
  private[gateway] val name: String,
  private[gateway] val source: Subgraph.Source[R],
  private[gateway] val lookups: List[Lookup],
  private[gateway] val transformations: List[SchemaTransformation]
) {

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
}

object Subgraph {

  /**
   * Describes an ordinary remote GraphQL graph whose schema is acquired through introspection.
   */
  def graphql(name: String, endpoint: Uri): Subgraph[Any] =
    graphql(name, endpoint, RemoteGraphQLConfig.default)

  /**
   * Describes an ordinary remote GraphQL graph with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: Uri, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Acquired, federation = false, config = config)

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL.
   */
  def graphql(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: Uri, schema: String, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Sdl(schema), federation = false, config = config)

  /**
   * Describes an ordinary remote GraphQL graph from an already parsed schema document.
   */
  def graphql(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    graphql(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes an ordinary remote GraphQL graph from a parsed document with remote GraphQL configuration.
   */
  def graphql[R](name: String, endpoint: Uri, schema: Document, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Parsed(schema), federation = false, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from pinned SDL.
   */
  def federation(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    federation(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph from pinned SDL with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: Uri, schema: String, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Sdl(schema), federation = true, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from an already parsed schema document.
   */
  def federation(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    federation(name, endpoint, schema, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph from a parsed document with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: Uri, schema: Document, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Parsed(schema), federation = true, config = config)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph whose schema is acquired through `_service`.
   */
  def federation(name: String, endpoint: Uri): Subgraph[Any] =
    federation(name, endpoint, RemoteGraphQLConfig.default)

  /**
   * Describes a Federation subgraph with remote GraphQL configuration.
   */
  def federation[R](name: String, endpoint: Uri, config: RemoteGraphQLConfig[R]): Subgraph[R] =
    remote(name, endpoint, SchemaInput.Acquired, federation = true, config = config)

  /**
   * Describes an in-process Caliban graph whose environment is supplied when the gateway executes.
   */
  def local[R](name: String, graph: GraphQL[R]): Subgraph[R] =
    new Subgraph[R](name, Source.Local(graph), Nil, Nil)

  private def remote[R](
    name: String,
    endpoint: Uri,
    schema: SchemaInput,
    federation: Boolean,
    config: RemoteGraphQLConfig[R]
  ): Subgraph[R] =
    new Subgraph[R](name, Source.Remote(endpoint, schema, federation, config), Nil, Nil)

  private[gateway] sealed trait Source[-R] {
    def isRemote: Boolean = this match {
      case _: Source.Remote[_] => true
      case _                   => false
    }
  }

  private[gateway] object Source {
    final case class Remote[R](
      endpoint: Uri,
      schema: SchemaInput,
      federation: Boolean,
      config: RemoteGraphQLConfig[R]
    ) extends Source[R]
    final case class Local[R](graph: GraphQL[R]) extends Source[R]
  }
}

private[gateway] sealed trait SchemaInput

private[gateway] object SchemaInput {
  final case class Sdl(value: String)      extends SchemaInput
  final case class Parsed(value: Document) extends SchemaInput
  case object Acquired                     extends SchemaInput
}
