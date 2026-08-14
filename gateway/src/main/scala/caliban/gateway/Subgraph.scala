package caliban.gateway

import caliban.CalibanError
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import sttp.model.Uri
import zio.{ IO, ZIO }

/**
 * A named remote GraphQL graph with a schema pinned by the application.
 */
final class Subgraph[-R] private[gateway] (
  private[gateway] val name: String,
  private[gateway] val endpoint: Uri,
  private[gateway] val schema: SchemaInput
)

object Subgraph {

  /**
   * Describes an ordinary remote GraphQL graph from pinned SDL.
   */
  def graphql(name: String, endpoint: Uri, schema: String): Subgraph[Any] =
    new Subgraph[Any](name, endpoint, SchemaInput.Sdl(schema))

  /**
   * Describes an ordinary remote GraphQL graph from an already parsed schema document.
   */
  def graphql(name: String, endpoint: Uri, schema: Document): Subgraph[Any] =
    new Subgraph[Any](name, endpoint, SchemaInput.Parsed(schema))
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
