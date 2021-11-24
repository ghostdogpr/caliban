package caliban.parsing.adt

import caliban.{ InputValue, Value }

case class Directive(name: String, arguments: Map[String, InputValue] = Map.empty, index: Int = 0)

object Directives {
  def isDeprecated(directives: List[Directive]): Boolean =
    directives.exists(_.name == "deprecated")

  def deprecationReason(directives: List[Directive]): Option[String] =
    directives.collectFirst {
      case f if f.name == "deprecated" =>
        f.arguments
          .get("reason")
          .flatMap(_ match {
            case Value.StringValue(value) => Some(value)
            case _                        => None
          })
    }.flatten
}
