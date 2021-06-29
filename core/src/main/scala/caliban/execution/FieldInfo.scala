package caliban.execution

import caliban.parsing.adt.Directive

case class FieldInfo(
  name: String,
  details: Field,
  path: List[Either[String, Int]],
  directives: List[Directive] = Nil
)
