package caliban.parsing

import caliban.GraphQLRequest
import caliban.InputValue.ListValue
import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.ListType
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.schema.RootType
import caliban.{ InputValue, Value }
import zio.{ IO, UIO }

object VariablesUpdater {
  def updateVariables(
    req: GraphQLRequest,
    doc: Document,
    rootType: RootType
  ): GraphQLRequest = {
    val variableDefinitions = doc.operationDefinitions.flatMap(_.variableDefinitions)
    val updated             = req.variables.getOrElse(Map.empty).map { case (key, value) =>
      val v =
        variableDefinitions
          .find(_.name == key)
          .map { definition: VariableDefinition =>
            rewriteValues(value, definition.variableType, rootType)
          }
          .getOrElse(value)

      key -> v
    }

    req.copy(variables = Some(updated))
  }

  def rewriteValues(value: InputValue, `type`: Type, rootType: RootType): InputValue =
    `type` match {
      case ListType(ofType, nonNull) =>
        value match {
          case ListValue(values) =>
            ListValue(values.map(v => rewriteValues(v, ofType, rootType)))
          case _                 => value
        }
      case NamedType(name, nonNull)  =>
        rootType.types.get(name).map(t => resolveEnumValues(value, t, rootType)).getOrElse(value)
    }

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  private def resolveEnumValues(
    value: InputValue,
    typ: __Type,
    rootType: RootType
  ): InputValue =
    typ.kind match {
      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val defs = typ.inputFields.getOrElse(List.empty)
            InputValue.ObjectValue(fields.map { case (k, v) =>
              val updated =
                defs.find(_.name == k).map(field => resolveEnumValues(v, field.`type`(), rootType)).getOrElse(value)

              (k, updated)
            })
          case _                              =>
            value
        }

      case __TypeKind.LIST =>
        value match {
          case ListValue(values) =>
            typ.ofType
              .map(innerType => ListValue(values.map(value => resolveEnumValues(value, innerType, rootType))))
              .getOrElse(value)
          case _                 => value
        }

      case __TypeKind.NON_NULL =>
        typ.ofType
          .map(innerType => resolveEnumValues(value, innerType, rootType))
          .getOrElse(value)

      case __TypeKind.ENUM =>
        value match {
          case StringValue(value) => Value.EnumValue(value)
          case _                  => value
        }
      case _               =>
        value
    }
}
