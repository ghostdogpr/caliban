package caliban.parsing

import caliban.InputValue.ListValue
import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt._
import caliban.schema.RootType
import caliban.{ GraphQLRequest, InputValue, Value }

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
          .map { definition =>
            rewriteValues(value, definition.variableType, rootType)
          }
          .getOrElse(value)

      key -> v
    }

    req.copy(variables = Some(updated))
  }

  private def rewriteValues(value: InputValue, `type`: Type, rootType: RootType): InputValue =
    `type` match {
      case ListType(ofType, _) =>
        value match {
          case ListValue(values) =>
            ListValue(values.map(v => rewriteValues(v, ofType, rootType)))
          case _                 => value
        }
      case NamedType(name, _)  =>
        rootType.types.get(name).map(t => coerceValues(value, t, rootType)).getOrElse(value)
    }

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  // Same thing with Int into Float.
  private def coerceValues(
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
                defs.find(_.name == k).map(field => coerceValues(v, field.`type`(), rootType)).getOrElse(value)

              (k, updated)
            })
          case _                              =>
            value
        }

      case __TypeKind.LIST =>
        value match {
          case ListValue(values) =>
            typ.ofType
              .map(innerType => ListValue(values.map(value => coerceValues(value, innerType, rootType))))
              .getOrElse(value)
          case _                 => value
        }

      case __TypeKind.NON_NULL =>
        typ.ofType
          .map(innerType => coerceValues(value, innerType, rootType))
          .getOrElse(value)

      case __TypeKind.ENUM                                 =>
        value match {
          case StringValue(value) => Value.EnumValue(value)
          case _                  => value
        }
      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case IntValue.IntNumber(value)    => Value.FloatValue(value)
          case IntValue.LongNumber(value)   => Value.FloatValue(value)
          case IntValue.BigIntNumber(value) => Value.FloatValue(BigDecimal(value))
          case _                            => value
        }
      case _                                               =>
        value
    }
}
