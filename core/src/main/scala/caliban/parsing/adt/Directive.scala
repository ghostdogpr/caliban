package caliban.parsing.adt

import caliban.{ InputValue, Value }

case class Directive(name: String, arguments: Map[String, InputValue] = Map.empty, index: Int = 0)

object Directives {

  val LazyDirective      = "lazy"
  val TypesafeDirective  = "typesafe"
  val DeprecatedDiretive = "deprecated"

  def isDeprecated(directives: List[Directive]): Boolean =
    directives.exists(_.name == DeprecatedDiretive)

  def deprecationReason(directives: List[Directive]): Option[String] =
    findDirective(directives, DeprecatedDiretive, "reason")

  def isTypesafe(directives: List[Directive]): Boolean          =
    directives.exists(_.name == TypesafeDirective)
  def typesafeName(directives: List[Directive]): Option[String] =
    Directives
      .findDirective(directives, TypesafeDirective, "name")

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
