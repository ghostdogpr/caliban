package caliban.parsing.adt

import caliban.parsing.adt.Type.{ FieldDefinition, NamedType }

sealed trait Definition

object Definition {
  sealed trait ExecutableDefinition extends Definition
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
  }

  sealed trait TypeSystemDefinition extends Definition
  object TypeSystemDefinition {
    case class ObjectTypeDefinition(name: String, fields: List[FieldDefinition]) extends TypeSystemDefinition
    case class EnumTypeDefinition(
      description: Option[String],
      name: String,
      directives: List[Directive],
      enumValuesDefinition: List[EnumValueDefinition]
    ) extends TypeSystemDefinition

    case class EnumValueDefinition(description: Option[String], enumValue: String, directives: List[Directive])
  }

}
