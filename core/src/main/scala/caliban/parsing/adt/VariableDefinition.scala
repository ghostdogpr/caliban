package caliban.parsing.adt

import caliban.InputValue

final case class VariableDefinition(
  name: String,
  variableType: Type,
  defaultValue: Option[InputValue],
  directives: List[Directive]
)
