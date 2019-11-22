package caliban.parsing.adt

import caliban.InputValue

case class VariableDefinition(
  name: String,
  variableType: Type,
  defaultValue: Option[InputValue],
  directives: List[Directive]
)
