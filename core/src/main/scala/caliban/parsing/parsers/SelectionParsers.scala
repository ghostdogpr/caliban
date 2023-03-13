package caliban.parsing.parsers

import caliban.InputValue
import caliban.parsing.adt.Selection._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import fastparse._

import scala.annotation.nowarn

@nowarn("msg=NoWhitespace") // False positive warning in Scala 2.x
private[caliban] trait SelectionParsers extends ValueParsers {
  def alias(implicit ev: P[Any]): P[String] = P(name ~ ":")

  def argument(implicit ev: P[Any]): P[(String, InputValue)]     = P(name ~ ":" ~ value)
  def arguments(implicit ev: P[Any]): P[Map[String, InputValue]] = P("(" ~/ argument.rep ~ ")").map(_.toMap)

  def directive(implicit ev: P[Any]): P[Directive]        = P(Index ~ "@" ~ name ~ arguments.?).map {
    case (index, name, arguments) =>
      Directive(name, arguments.getOrElse(Map()), index)
  }
  def directives(implicit ev: P[Any]): P[List[Directive]] = P(directive.rep).map(_.toList)

  def selection(implicit ev: P[Any]): P[Selection]          = P(field | fragmentSpread | inlineFragment)
  def selectionSet(implicit ev: P[Any]): P[List[Selection]] = P("{" ~/ selection.rep ~ "}").map(_.toList)

  def namedType(implicit ev: P[Any]): P[NamedType] = P(name.filter(_ != "null")).map(NamedType(_, nonNull = false))
  def listType(implicit ev: P[Any]): P[ListType]   = P("[" ~ type_ ~ "]").map(t => ListType(t, nonNull = false))

  def nonNullType(implicit ev: P[Any]): P[Type] = P((namedType | listType) ~ "!").map {
    case t: NamedType => t.copy(nonNull = true)
    case t: ListType  => t.copy(nonNull = true)
  }
  def type_(implicit ev: P[Any]): P[Type]       = P(nonNullType | namedType | listType)

  def field(implicit ev: P[Any]): P[Field] =
    P(Index ~ alias.? ~ name ~ arguments.? ~ directives.? ~ selectionSet.?).map {
      case (index, alias, name, args, dirs, sels) =>
        Field(
          alias,
          name,
          args.getOrElse(Map()),
          dirs.getOrElse(Nil),
          sels.getOrElse(Nil),
          index
        )
    }

  def fragmentName(implicit ev: P[Any]): P[String] = P(name).filter(_ != "on")

  def fragmentSpread(implicit ev: P[Any]): P[FragmentSpread] = P("..." ~ fragmentName ~ directives).map {
    case (name, dirs) =>
      FragmentSpread(name, dirs)
  }

  def typeCondition(implicit ev: P[Any]): P[NamedType] = P("on" ~/ namedType)

  def inlineFragment(implicit ev: P[Any]): P[InlineFragment] =
    P("..." ~ typeCondition.? ~ directives ~ selectionSet).map { case (typeCondition, dirs, sel) =>
      InlineFragment(typeCondition, dirs, sel)
    }
}
