package caliban.execution

import caliban.PathValue
import caliban.introspection.adt.__Type
import caliban.parsing.adt.Directive

case class FieldInfo(
  name: String,
  details: Field,
  path: List[PathValue],
  directives: List[Directive] = Nil,
  parent: Option[__Type]
)
