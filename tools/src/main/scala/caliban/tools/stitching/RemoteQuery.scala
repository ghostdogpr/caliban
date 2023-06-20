package caliban.tools.stitching

import caliban.execution.Field
import caliban.{ GraphQLRequest, InputValue }

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
    def render(r: RemoteMutation): String = {
      val s = renderField(r.field)
      if (r.field.name.isEmpty) s"mutation $s" else s"mutation { $s }"
    }
    def render(r: RemoteQuery): String    = {
      val s = renderField(r.field)
      if (r.field.name.isEmpty) s"query $s" else s"query { $s }"
    }

    def renderField(field: Field, ignoreArguments: Boolean = false): String = {
      val children = renderFields(field)
      val args     = if (ignoreArguments) "" else renderArguments(field.arguments)
      val alias    = field.alias.map(a => s"$a: ").getOrElse("")
      val str      = s"$alias${field.name}$args$children"

      field.targets
        .map(typeConditions => typeConditions.map(condition => s"...on $condition { $str }").mkString("\n"))
        .getOrElse(str)
    }

    private def renderFields(f: Field): String =
      if (f.fields.isEmpty) ""
      else {
        val isRoot = f.parentType.isEmpty
        val fields = if (isRoot) f.fields.map(renderField(_)) else "__typename" :: f.fields.map(renderField(_))
        fields.mkString(" { ", " ", " }")
      }

    private def renderArguments(args: Map[String, InputValue]): String =
      if (args.isEmpty) ""
      else
        args.map { case (k, v) =>
          k.split('.').toList.reverse match {
            case Nil          => ""
            case head :: tail => tail.foldLeft(s"$head: ${v.toInputString}")((acc, elem) => s"$elem: { $acc }")
          }
        }.mkString("(", ", ", ")")
  }
}
