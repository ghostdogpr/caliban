package caliban.parsing.parsers

import caliban.InputValue
import caliban.parsing.adt.Selection._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import fastparse._

import scala.annotation.nowarn

@nowarn("msg=NoWhitespace") // False positive warning in Scala 2.x
private[caliban] trait SelectionParsers extends ValueParsers {

  @deprecated("Kept for bincompat only, scheduled to be removed")
  def alias(implicit ev: P[Any]): P[String] = name ~ ":"
  def aliasOrName(implicit ev: P[Any]): P[String] = ":" ~/ name

  def argument(implicit ev: P[Any]): P[(String, InputValue)]     = name ~ ":" ~ value
  def arguments(implicit ev: P[Any]): P[Map[String, InputValue]] = ("(" ~/ argument.rep ~ ")").map(_.toMap)

  def directive(implicit ev: P[Any]): P[Directive]        = ("@" ~~/ Index ~~ name ~ arguments.?).map {
    case (index, name, arguments) =>
      Directive(name, arguments.getOrElse(Map()), index - 1)
  }
  def directives(implicit ev: P[Any]): P[List[Directive]] = directive.rep.map(_.toList)

  def selection(implicit ev: P[Any]): P[Selection]          = !"}" ~~ (field | fragmentSpread | inlineFragment)
  def selectionSet(implicit ev: P[Any]): P[List[Selection]] = ("{" ~/ selection.rep ~ "}").map(_.toList)

  def namedType(implicit ev: P[Any]): P[NamedType] = name.filter(_ != "null").map(NamedType(_, nonNull = false))
  def listType(implicit ev: P[Any]): P[ListType]   = ("[" ~ type_ ~ "]").map(t => ListType(t, nonNull = false))

  @deprecated("Kept for bincompat only, scheduled to be removed")
  def nonNullType(implicit ev: P[Any]): P[Type] = ((namedType | listType) ~ "!").map {
    case t: NamedType => t.copy(nonNull = true)
    case t: ListType  => t.copy(nonNull = true)
  }

  def type_(implicit ev: P[Any]): P[Type] = ((namedType | listType) ~ "!".!.?).map {
    case (t: NamedType, nn) => if (nn.isDefined) t.copy(nonNull = true) else t
    case (t: ListType, nn)  => if (nn.isDefined) t.copy(nonNull = true) else t
  }

  def field(implicit ev: P[Any]): P[Field] =
    (Index ~ name ~ aliasOrName.? ~ arguments.? ~ directives.? ~ selectionSet.?).map {
      case (index, alias, Some(name), args, dirs, sels) =>
        Field(Some(alias), name, args.getOrElse(Map()), dirs.getOrElse(Nil), sels.getOrElse(Nil), index)
      case (index, name, _, args, dirs, sels)           =>
        Field(None, name, args.getOrElse(Map()), dirs.getOrElse(Nil), sels.getOrElse(Nil), index)

    }

  def fragmentName(implicit ev: P[Any]): P[String] = name.filter(_ != "on")

  def fragmentSpread(implicit ev: P[Any]): P[FragmentSpread] = ("..." ~ fragmentName ~ directives).map {
    case (name, dirs) =>
      FragmentSpread(name, dirs)
  }

  def typeCondition(implicit ev: P[Any]): P[NamedType] = "on" ~/ namedType

  def inlineFragment(implicit ev: P[Any]): P[InlineFragment] =
    ("..." ~ typeCondition.? ~ directives ~ selectionSet).map { case (typeCondition, dirs, sel) =>
      InlineFragment(typeCondition, dirs, sel)
    }
}
