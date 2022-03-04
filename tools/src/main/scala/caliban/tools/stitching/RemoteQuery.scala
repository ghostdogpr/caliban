package caliban.tools.stitching

import caliban.{ GraphQLRequest, InputValue }
import caliban.execution.Field
import caliban.Value._
import caliban.InputValue._
import caliban.Value.FloatValue._
import caliban.Value.IntValue._
import caliban.parsing.adt.Selection
import caliban.parsing.adt.Selection.FragmentSpread
import caliban.parsing.adt.Selection.InlineFragment

case class RemoteQuery(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        RemoteQuery.QueryRenderer.render(self)
      )
    )
}

case class RemoteMutation(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        RemoteQuery.QueryRenderer.render(self)
      )
    )
}

object RemoteQuery {
  object QueryRenderer {
    def render(r: RemoteMutation): String = s"mutation { ${renderField(r.field)} }"
    def render(r: RemoteQuery): String    = s"query { ${renderField(r.field)} }"

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
}
