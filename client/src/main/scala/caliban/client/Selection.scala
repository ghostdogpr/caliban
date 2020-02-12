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
}
