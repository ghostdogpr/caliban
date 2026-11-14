package caliban.schema

import caliban.CalibanError.ValidationError
import caliban.introspection.Introspector
import caliban.introspection.adt.{ __Directive, __Type, __TypeKind, SchemaElement }
import caliban.parsing.adt.SchemaCoordinate
import caliban.parsing.adt.SchemaCoordinate._

case class RootType(
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  additionalTypes: List[__Type] = List.empty,
  additionalDirectives: List[__Directive] = List.empty,
  description: Option[String] = None
) {
  private val primitiveTypes: List[__Type] = List(Types.boolean, Types.int, Types.float, Types.string)

  val types: Map[String, __Type] =
    Types
      .collectRootTypes(additionalTypes ++ primitiveTypes, queryType :: mutationType.toList ::: subscriptionType.toList)
      .map(t => t.name.getOrElse("") -> t)
      .toMap

  def resolve(coordinate: SchemaCoordinate): Either[ValidationError, Option[SchemaElement]] = {
    def invalid(reason: String) =
      ValidationError(
        s"Invalid schema coordinate '$coordinate': $reason.",
        "A schema coordinate raises an error when its containing element cannot be found in the schema."
      )

    def namedType(name: String) = types.get(name).orElse(Types.builtInScalars.get(name))

    def directive(name: String) = (additionalDirectives ++ Introspector.builtInDirectives).find(_.name == name)

    def member(typeName: String, name: String): Either[ValidationError, Option[SchemaElement]] =
      namedType(typeName).toRight(invalid(s"type '$typeName' does not exist")).flatMap { t =>
        t.kind match {
          case __TypeKind.ENUM                          => Right(t.allEnumValues.find(_.name == name).map(SchemaElement.EnumValue(_)))
          case __TypeKind.INPUT_OBJECT                  => Right(Option(t.getInputFieldOrNull(name)).map(SchemaElement.InputValue(_)))
          case __TypeKind.OBJECT | __TypeKind.INTERFACE =>
            Right(Option(t.getFieldOrNull(name)).map(SchemaElement.Field(_)))
          case kind                                     => Left(invalid(s"type '$typeName' has kind $kind"))
        }
      }

    coordinate match {
      case TypeCoordinate(name)                             => Right(namedType(name).map(SchemaElement.Type(_)))
      case MemberCoordinate(typeName, name)                 => member(typeName, name)
      case ArgumentCoordinate(typeName, fieldName, name)    =>
        member(typeName, fieldName).flatMap {
          case Some(SchemaElement.Field(field)) =>
            Right(field.allArgs.find(_.name == name).map(SchemaElement.InputValue(_)))
          case _                                => Left(invalid(s"'$typeName.$fieldName' is not a field"))
        }
      case DirectiveCoordinate(name)                        => Right(directive(name).map(SchemaElement.Directive(_)))
      case DirectiveArgumentCoordinate(directiveName, name) =>
        directive(directiveName)
          .toRight(invalid(s"directive '@$directiveName' does not exist"))
          .map(_.allArgs.find(_.name == name).map(SchemaElement.InputValue(_)))
    }
  }
}
