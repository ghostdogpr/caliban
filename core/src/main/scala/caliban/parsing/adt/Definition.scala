package caliban.parsing.adt

import caliban.InputValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{
  EnumValueDefinition,
  FieldDefinition,
  InputValueDefinition
}
import caliban.parsing.adt.Type.NamedType

sealed trait Definition

object Definition {
  sealed trait ExecutableDefinition extends Definition
  object ExecutableDefinition {
    final case class OperationDefinition(
      operationType: OperationType,
      name: Option[String],
      variableDefinitions: List[VariableDefinition],
      directives: List[Directive],
      selectionSet: List[Selection]
    ) extends ExecutableDefinition

    final case class FragmentDefinition(
      name: String,
      typeCondition: NamedType,
      directives: List[Directive],
      selectionSet: List[Selection]
    ) extends ExecutableDefinition
  }

  sealed trait TypeSystemDefinition extends Definition
  object TypeSystemDefinition {

    final case class SchemaDefinition(
      directives: List[Directive],
      query: Option[String],
      mutation: Option[String],
      subscription: Option[String]
    ) extends TypeSystemDefinition

    final case class DirectiveDefinition(
      description: Option[String],
      name: String,
      args: List[InputValueDefinition],
      repeatable: Boolean,
      locations: Set[DirectiveLocation]
    ) extends TypeSystemDefinition

    sealed trait DirectiveLocation
    object DirectiveLocation {
      sealed trait ExecutableDirectiveLocation extends DirectiveLocation
      object ExecutableDirectiveLocation {
        case object QUERY               extends ExecutableDirectiveLocation
        case object MUTATION            extends ExecutableDirectiveLocation
        case object SUBSCRIPTION        extends ExecutableDirectiveLocation
        case object FIELD               extends ExecutableDirectiveLocation
        case object FRAGMENT_DEFINITION extends ExecutableDirectiveLocation
        case object FRAGMENT_SPREAD     extends ExecutableDirectiveLocation
        case object INLINE_FRAGMENT     extends ExecutableDirectiveLocation
      }
      sealed trait TypeSystemDirectiveLocation extends DirectiveLocation
      object TypeSystemDirectiveLocation {
        case object SCHEMA                 extends TypeSystemDirectiveLocation
        case object SCALAR                 extends TypeSystemDirectiveLocation
        case object OBJECT                 extends TypeSystemDirectiveLocation
        case object FIELD_DEFINITION       extends TypeSystemDirectiveLocation
        case object ARGUMENT_DEFINITION    extends TypeSystemDirectiveLocation
        case object INTERFACE              extends TypeSystemDirectiveLocation
        case object UNION                  extends TypeSystemDirectiveLocation
        case object ENUM                   extends TypeSystemDirectiveLocation
        case object ENUM_VALUE             extends TypeSystemDirectiveLocation
        case object INPUT_OBJECT           extends TypeSystemDirectiveLocation
        case object INPUT_FIELD_DEFINITION extends TypeSystemDirectiveLocation
        case object VARIABLE_DEFINITION    extends TypeSystemDirectiveLocation
      }
    }

    sealed trait TypeDefinition extends TypeSystemDefinition {
      def name: String
      def description: Option[String]
      def directives: List[Directive]
    }
    object TypeDefinition {

      final case class ObjectTypeDefinition(
        description: Option[String],
        name: String,
        implements: List[NamedType],
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeDefinition {
        override def toString: String = "Object"
      }

      final case class InterfaceTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeDefinition {
        override def toString: String = "Interface"
      }

      final case class InputObjectTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        fields: List[InputValueDefinition]
      ) extends TypeDefinition {
        override def toString: String = "Input Object"
      }

      final case class EnumTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        enumValuesDefinition: List[EnumValueDefinition]
      ) extends TypeDefinition {
        override def toString: String = "Enum"
      }

      final case class UnionTypeDefinition(
        description: Option[String],
        name: String,
        directives: List[Directive],
        memberTypes: List[String]
      ) extends TypeDefinition {
        override def toString: String = "Union"
      }

      final case class ScalarTypeDefinition(description: Option[String], name: String, directives: List[Directive])
          extends TypeDefinition {
        override def toString: String = "Scalar"
      }

      final case class InputValueDefinition(
        description: Option[String],
        name: String,
        ofType: Type,
        defaultValue: Option[InputValue],
        directives: List[Directive]
      )

      final case class FieldDefinition(
        description: Option[String],
        name: String,
        args: List[InputValueDefinition],
        ofType: Type,
        directives: List[Directive]
      )

      final case class EnumValueDefinition(description: Option[String], enumValue: String, directives: List[Directive])

    }

  }

  sealed trait TypeSystemExtension extends Definition

  object TypeSystemExtension {

    final case class SchemaExtension(
      directives: List[Directive],
      query: Option[String],
      mutation: Option[String],
      subscription: Option[String]
    ) extends TypeSystemExtension

    sealed trait TypeExtension extends TypeSystemExtension

    object TypeExtension {

      final case class ScalarTypeExtension(
        name: String,
        directives: List[Directive]
      ) extends TypeExtension

      final case class ObjectTypeExtension(
        name: String,
        implements: List[NamedType],
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeExtension

      final case class InterfaceTypeExtension(
        name: String,
        directives: List[Directive],
        fields: List[FieldDefinition]
      ) extends TypeExtension

      final case class UnionTypeExtension(
        name: String,
        directives: List[Directive],
        memberTypes: List[String]
      ) extends TypeExtension

      final case class EnumTypeExtension(
        name: String,
        directives: List[Directive],
        enumValuesDefinition: List[EnumValueDefinition]
      ) extends TypeExtension

      final case class InputObjectTypeExtension(
        name: String,
        directives: List[Directive],
        fields: List[InputValueDefinition]
      ) extends TypeExtension
    }
  }
}
