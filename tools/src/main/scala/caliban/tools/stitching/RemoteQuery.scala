package caliban.tools.stitching

import caliban.{ GraphQLRequest, InputValue }
import caliban.execution.Field
import caliban.Value._
import caliban.InputValue._
import caliban.Value.FloatValue._
import caliban.Value.IntValue._

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

      withCondition(field)(str)
    }

    private def withCondition(f: Field)(inner: String): String =
      f.condition.flatMap(_.headOption).map(x => s"...on $x { $inner }").getOrElse(inner)

    private def renderInputValue(v: InputValue): String =
      v match {
        case StringValue(value)      => s""""$value""""
        case ListValue(values)       => values.map(renderInputValue).mkString("[", ",", "]")
        case ObjectValue(fields)     =>
          fields.map({ case (k, v) => s"""$k: ${renderInputValue(v)}""" }).mkString("{ ", ", ", " }")
        case NullValue               => "null"
        case VariableValue(name)     => s"$$$name"
        case BigDecimalNumber(value) => s"$value"
        case BigIntNumber(value)     => s"$value"
        case BooleanValue(value)     => s"$value"
        case DoubleNumber(value)     => s"$value"
        case EnumValue(value)        => s"$value"
        case FloatNumber(value)      => s"$value"
        case IntNumber(value)        => s"$value"
        case LongNumber(value)       => s"$value"
      }

    private def renderFields(f: Field): String =
      if (f.fields.isEmpty) ""
      else
        f.fields.map(renderField(_)).mkString(" { ", " ", " }")

    private def renderArguments(args: Map[String, InputValue]): String =
      if (args.isEmpty) ""
      else
        args
          .map({ case (k, v) => s"$k: ${renderInputValue(v)}" })
          .mkString("(", ", ", ")")
  }
}
