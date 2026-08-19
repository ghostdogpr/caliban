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
  def graphql(
    name: String,
    endpoint: Uri,
    acquisition: SchemaAcquisition = SchemaAcquisition.default
  ): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Acquired(acquisition), federation = false), Nil, Nil)

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL.
   */
  def graphql(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Sdl(schema), federation = false), Nil, Nil)

  /**
   * Describes an ordinary remote GraphQL graph from an already parsed schema document.
   */
  def graphql(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Parsed(schema), federation = false), Nil, Nil)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from pinned SDL.
   */
  def federation(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Sdl(schema), federation = true), Nil, Nil)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from an already parsed schema document.
   */
  def federation(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Parsed(schema), federation = true), Nil, Nil)

  /**
   * Describes a Federation-enabled remote GraphQL subgraph whose schema is acquired through `_service`.
   */
  def federation(
    name: String,
    endpoint: Uri,
    acquisition: SchemaAcquisition = SchemaAcquisition.default
  ): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Acquired(acquisition), federation = true), Nil, Nil)

  /**
   * Describes an in-process Caliban graph whose environment is supplied when the gateway executes.
   */
  def local[R](name: String, graph: GraphQL[R]): Subgraph[R] =
    new Subgraph[R](name, Source.Local(graph), Nil, Nil)

  private[gateway] sealed trait Source[-R]

  private[gateway] object Source {
    final case class Remote(endpoint: Uri, schema: SchemaInput, federation: Boolean) extends Source[Any]
    final case class Local[R](graph: GraphQL[R])                                     extends Source[R]
  }
}

private[gateway] sealed trait SchemaInput

private[gateway] object SchemaInput {
  final case class Sdl(value: String)                  extends SchemaInput
  final case class Parsed(value: Document)             extends SchemaInput
  final case class Acquired(config: SchemaAcquisition) extends SchemaInput
}
