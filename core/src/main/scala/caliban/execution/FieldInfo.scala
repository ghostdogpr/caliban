package caliban.execution

import caliban.parsing.adt.Directive

final case class FieldInfo(
  name: String,
  details: Field,
  path: List[Either[String, Int]],
  directives: List[Directive] = Nil
)
