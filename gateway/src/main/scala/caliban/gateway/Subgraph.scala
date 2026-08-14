package caliban.gateway

import caliban.{ CalibanError, GraphQL }
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import sttp.model.Uri
import zio.{ IO, ZIO }

/**
 * A named GraphQL graph that participates in gateway composition and execution.
 */
final class Subgraph[-R] private[gateway] (
  private[gateway] val name: String,
  private[gateway] val source: Subgraph.Source[R]
)

object Subgraph {

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL.
   */
  def graphql(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Sdl(schema), federation = false))

  /**
   * Describes an ordinary remote GraphQL graph from an already parsed schema document.
   */
  def graphql(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Parsed(schema), federation = false))

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from pinned SDL.
   */
  def federation(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Sdl(schema), federation = true))

  /**
   * Describes a Federation-enabled remote GraphQL subgraph from an already parsed schema document.
   */
  def federation(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    new Subgraph[Any](name, Source.Remote(endpoint, SchemaInput.Parsed(schema), federation = true))

  /**
   * Describes an in-process Caliban graph whose environment is supplied when the gateway executes.
   */
  def local[R](name: String, graph: GraphQL[R]): Subgraph[R] =
    new Subgraph[R](name, Source.Local(graph))

  private[gateway] sealed trait Source[-R]

  private[gateway] object Source {
    final case class Remote(endpoint: Uri, schema: SchemaInput, federation: Boolean) extends Source[Any]
    final case class Local[R](graph: GraphQL[R])                                     extends Source[R]
  }
}

private[gateway] sealed trait SchemaInput {
  final def document: IO[CalibanError.ParsingError, Document] =
    this match {
      case SchemaInput.Sdl(value)    => ZIO.fromEither(Parser.parseQuery(value))
      case SchemaInput.Parsed(value) => ZIO.succeed(value)
    }
}

private[gateway] object SchemaInput {
  final case class Sdl(value: String)      extends SchemaInput
  final case class Parsed(value: Document) extends SchemaInput
}
