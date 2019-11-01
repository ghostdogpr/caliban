package caliban.parsing.adt

import caliban.parsing.adt.Type.{ FieldDefinition, NamedType }

sealed trait ExecutableDefinition

object ExecutableDefinition {

  case class OperationDefinition(
    operationType: OperationType,
    name: Option[String],
    variableDefinitions: List[VariableDefinition],
    directives: List[Directive],
    selectionSet: List[Selection]
  ) extends ExecutableDefinition

  case class FragmentDefinition(
    name: String,
    typeCondition: NamedType,
    directives: List[Directive],
    selectionSet: List[Selection]
  ) extends ExecutableDefinition

  case class TypeDefinition(name: String, children: List[FieldDefinition]) extends ExecutableDefinition

}
