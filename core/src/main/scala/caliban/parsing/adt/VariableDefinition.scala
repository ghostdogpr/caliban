package caliban.parsing.adt

case class VariableDefinition(
  name: String,
  variableType: Type,
  defaultValue: Option[Value],
  directives: List[Directive]
)
