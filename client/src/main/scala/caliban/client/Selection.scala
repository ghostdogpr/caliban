package caliban.client

sealed trait Selection

object Selection {
  case class InlineFragment(onType: String, selectionSet: List[Selection]) extends Selection

  case class Field(
    alias: Option[String],
    name: String,
    arguments: List[Argument[_]],
    directives: List[Directive],
    selectionSet: List[Selection],
    code: Int
  ) extends Selection

  case class FragmentSpread(
    name: Option[String],
    on: String,
    selectionSet: List[Selection],
    directives: List[Directive]
  ) extends Selection {
    val code: Int = hashCode()
  }

  case class Directive(name: String, arguments: List[Argument[_]] = Nil)

}
