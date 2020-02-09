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

    case class SchemaDefinition(
      directives: List[Directive],
      query: Option[String],
      mutation: Option[String],
      subscription: Option[String]
    ) extends TypeSystemDefinition

    sealed trait TypeDefinition extends TypeSystemDefinition
    object TypeDefinition {

      case class ObjectTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeDefinition

      case class EnumTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        enumValuesDefinition: List[EnumValueDefinition]
      ) extends TypeDefinition

      case class UnionTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        memberTypes: List[String]
      ) extends TypeDefinition

      case class ScalarTypeDefinition(description: Option[String], name: String, directives: List[Directive])
          extends TypeDefinition

      case class EnumValueDefinition(description: Option[String], enumValue: String, directives: List[Directive])

    }

  }
}
