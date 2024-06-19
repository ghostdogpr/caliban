package caliban.parsing.adt

import caliban.{ InputValue, Value }

case class Directive(
  name: String,
  arguments: Map[String, InputValue] = Map.empty,
  index: Int = 0,
  isIntrospectable: Boolean = true
)

object Directives {

  final val Defer               = "defer"
  final val DeprecatedDirective = "deprecated"
  final val LazyDirective       = "lazy"
  final val NewtypeDirective    = "newtype"
  final val OneOf               = "oneOf"
  final val Stream              = "stream"

  // We prefix these with `_caliban_tag_` to avoid conflicts with user-defined directives
  private[caliban] object internal {
    final val Tag = "_caliban_tag"

    def tags(directives: List[Directive]): Set[String] =
      directives.collect { case Directive(Tag, args, _, _) if args.nonEmpty => args.keySet }
        .foldLeft(Set.empty[String])(_ ++ _)

  }

  def isDeprecated(directives: List[Directive]): Boolean =
    directives.exists(_.name == DeprecatedDirective)

  def deprecationReason(directives: List[Directive]): Option[String] =
    findDirective(directives, DeprecatedDirective, "reason")

  def isNewType(directives: List[Directive]): Boolean =
    directives.exists(_.name == NewtypeDirective)

  def newTypeName(directives: List[Directive]): Option[String] =
    findDirective(directives, NewtypeDirective, "name")

  def isOneOf(directives: List[Directive]): Boolean =
    directives.exists(_.name == OneOf)

  private def findDirective(directives: List[Directive], name: String, argument: String): Option[String] =
    directives.collectFirst {
      case f if f.name == name =>
        f.arguments
          .get(argument)
          .flatMap(_ match {
            case Value.StringValue(value) => Some(value)
            case _                        => None
          })
    }.flatten

}
