package caliban.parsing.adt

import caliban.{ InputValue, Value }

case class Directive(name: String, arguments: Map[String, InputValue] = Map.empty, index: Int = 0)

object Directives {

  val LazyDirective       = "lazy"
  val NewtypeDirective    = "newtype"
  val DeprecatedDirective = "deprecated"

  def isDeprecated(directives: List[Directive]): Boolean =
    directives.exists(_.name == DeprecatedDirective)

  def deprecationReason(directives: List[Directive]): Option[String] =
    findDirective(directives, DeprecatedDirective, "reason")

  def isNewType(directives: List[Directive]): Boolean          =
    directives.exists(_.name == NewtypeDirective)
  def newTypeName(directives: List[Directive]): Option[String] =
    findDirective(directives, NewtypeDirective, "name")

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
