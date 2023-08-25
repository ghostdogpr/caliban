package caliban.tools.stitching

import caliban._
import caliban.execution.Field
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Document, OperationType }
import caliban.rendering.DocumentRenderer

case class RemoteQuery(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(
      query = Some(DocumentRenderer.renderCompact(toDocument))
    )

  def toDocument: Document = RemoteQuery.toDocument(OperationType.Query, field)
}

case class RemoteMutation(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        DocumentRenderer.renderCompact(toDocument)
      )
    )

  def toDocument: Document = RemoteQuery.toDocument(OperationType.Mutation, field)
}

object RemoteQuery {
  object QueryRenderer {
    def render(r: RemoteMutation): String =
      DocumentRenderer.renderCompact(r.toDocument)
    def render(r: RemoteQuery): String    =
      DocumentRenderer.renderCompact(r.toDocument)
  }

  def toDocument(operationType: OperationType, field: Field): Document =
    Document(
      List(
        OperationDefinition(
          operationType,
          None,
          Nil,
          Nil,
          field.fields.map(_.toSelection)
        )
      ),
      SourceMapper.empty
    )
}
