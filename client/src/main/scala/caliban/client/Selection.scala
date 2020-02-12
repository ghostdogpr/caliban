package caliban.client

sealed trait Selection

object Selection {
  case class InlineFragment(onType: String, selectionSet: List[Selection]) extends Selection
  case class Field(
    alias: Option[String],
    name: String,
    arguments: List[Argument[_]],
    directives: List[Directive],
    selectionSet: List[Selection]
  ) extends Selection

  case class Directive(name: String, arguments: List[Argument[_]] = Nil) {
    def toGraphQL: String = {
      val args = arguments.map(_.toGraphQL).mkString(",")
      s"@$name($args)"
    }
  }
}
