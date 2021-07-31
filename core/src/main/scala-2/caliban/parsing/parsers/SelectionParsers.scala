package caliban.parsing.parsers

import caliban.InputValue
import caliban.parsing.adt.Selection._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import fastparse._

private[caliban] trait SelectionParsers extends ValueParsers {
  def alias[_: P]: P[String] = P(name ~ ":")

  def argument[_: P]: P[(String, InputValue)]     = P(name ~ ":" ~ value)
  def arguments[_: P]: P[Map[String, InputValue]] = P("(" ~/ argument.rep ~ ")").map(_.toMap)

  def directive[_: P]: P[Directive]        = P(Index ~ "@" ~ name ~ arguments.?).map { case (index, name, arguments) =>
    Directive(name, arguments.getOrElse(Map()), index)
  }
  def directives[_: P]: P[List[Directive]] = P(directive.rep).map(_.toList)

  def selection[_: P]: P[Selection]          = P(field | fragmentSpread | inlineFragment)
  def selectionSet[_: P]: P[List[Selection]] = P("{" ~/ selection.rep ~ "}").map(_.toList)

  def namedType[_: P]: P[NamedType] = P(name.filter(_ != "null")).map(NamedType(_, nonNull = false))
  def listType[_: P]: P[ListType]   = P("[" ~ type_ ~ "]").map(t => ListType(t, nonNull = false))

  def nonNullType[_: P]: P[Type] = P((namedType | listType) ~ "!").map {
    case t: NamedType => t.copy(nonNull = true)
    case t: ListType  => t.copy(nonNull = true)
  }
  def type_[_: P]: P[Type]       = P(nonNullType | namedType | listType)

  def field[_: P]: P[Field] = P(Index ~ alias.? ~ name ~ arguments.? ~ directives.? ~ selectionSet.?).map {
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

  def fragmentName[_: P]: P[String] = P(name).filter(_ != "on")

  def fragmentSpread[_: P]: P[FragmentSpread] = P("..." ~ fragmentName ~ directives).map { case (name, dirs) =>
    FragmentSpread(name, dirs)
  }

  def typeCondition[_: P]: P[NamedType] = P("on" ~/ namedType)

  def inlineFragment[_: P]: P[InlineFragment] = P("..." ~ typeCondition.? ~ directives ~ selectionSet).map {
    case (typeCondition, dirs, sel) => InlineFragment(typeCondition, dirs, sel)
  }
}
