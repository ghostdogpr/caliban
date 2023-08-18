package caliban.tools.stitching

import caliban.{ GraphQLRequest, InputValue }
import caliban.execution.Field
import caliban.Value._
import caliban.InputValue._
import caliban.Value.FloatValue._
import caliban.Value.IntValue._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Document, OperationType, Selection }
import caliban.rendering.DocumentRenderer

case class RemoteQuery(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(
      query = Some(DocumentRenderer.render(toDocument))
    )

  def toDocument: Document = RemoteQuery.toDocument(OperationType.Query, field)
}

case class RemoteMutation(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        DocumentRenderer.render(toDocument)
      )
    )

  def toDocument: Document = RemoteQuery.toDocument(OperationType.Mutation, field)
}

object RemoteQuery {
  object QueryRenderer {
    def render(r: RemoteMutation): String =
      DocumentRenderer.render(toDocument(OperationType.Mutation, r.field))
    def render(r: RemoteQuery): String    =
      DocumentRenderer.render(toDocument(OperationType.Query, r.field))
  }

  def toDocument(operationType: OperationType, field: Field): Document = {
    def loop(f: Field): Selection.Field =
      Selection.Field(
        f.alias,
        f.name,
        f.arguments,
        f.directives,
        f.fields.map(loop),
        0
      )

    Document(
      List(
        OperationDefinition(
          operationType,
          None,
          Nil,
          Nil,
          List(loop(field))
        )
      ),
      SourceMapper.empty
    )
  }
}
