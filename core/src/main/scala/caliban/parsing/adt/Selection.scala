package caliban.parsing.adt

import caliban.InputValue
import caliban.parsing.adt.Type.NamedType

sealed trait Selection

object Selection {

  final case class Field(
    alias: Option[String],
    name: String,
    arguments: Map[String, InputValue],
    directives: List[Directive],
    selectionSet: List[Selection],
    index: Int
  ) extends Selection

  final case class FragmentSpread(name: String, directives: List[Directive]) extends Selection

  final case class InlineFragment(
    typeCondition: Option[NamedType],
    dirs: List[Directive],
    selectionSet: List[Selection]
  ) extends Selection

}
