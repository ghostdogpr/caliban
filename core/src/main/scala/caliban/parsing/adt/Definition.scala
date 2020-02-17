package caliban.parsing.adt

import caliban.InputValue
import caliban.parsing.adt.Type.NamedType

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
        implements: List[NamedType],
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeDefinition

      case class InterfaceTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeDefinition

      case class InputObjectTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        fields: List[InputValueDefinition]
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

      case class InputValueDefinition(
        description: Option[String],
        name: String,
        ofType: Type,
        defaultValue: Option[InputValue],
        directives: List[Directive]
      )

      case class FieldDefinition(
        description: Option[String],
        name: String,
        args: List[InputValueDefinition],
        ofType: Type,
        directives: List[Directive]
      )

      case class EnumValueDefinition(description: Option[String], enumValue: String, directives: List[Directive])

    }

  }
}
