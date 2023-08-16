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

    private def renderField(field: Field): String = {
      val children = renderFields(field)
      val args     = renderArguments(field.arguments)
      val alias    = field.alias.map(a => s"$a: ").getOrElse("")
      val str      = s"$alias${field.name}$args$children"

      field.targets
        .map(typeConditions => typeConditions.map(condition => s"...on $condition { $str }").mkString("\n"))
        .getOrElse(str)
    }

    private def renderFields(f: Field): String =
      if (f.fields.isEmpty) ""
      else
        f.fields.map(renderField).mkString(" { ", " ", " }")

    private def renderArguments(args: Map[String, InputValue]): String =
      if (args.isEmpty) "" else args.map { case (k, v) => s"$k: ${v.toInputString}" }.mkString("(", ", ", ")")
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
