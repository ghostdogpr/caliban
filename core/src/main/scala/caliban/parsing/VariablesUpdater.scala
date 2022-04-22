package caliban.parsing

import caliban.InputValue.ListValue
import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt._
import caliban.schema.RootType
import caliban.{ GraphQLRequest, InputValue, Value }
import zio._
import caliban.CalibanError.ValidationError

object VariablesUpdater {
  def prepare(
    req: GraphQLRequest,
    doc: Document,
    rootType: RootType
  ): IO[ValidationError, GraphQLRequest] = {
    val variableDefinitions = doc.operationDefinitions.flatMap(_.variableDefinitions)
    var variables           = req.variables.getOrElse(Map.empty)

    IO.foldLeft(variableDefinitions)(Map.empty[String, InputValue]) { case (coercedValues, definition) =>
      IO.fromEither(isInputType(definition.variableType, rootType))
        .mapError(e => ValidationError(e, "")) *> {
        val value =
          variables
            .get(definition.name)
            .map(rewriteValues(_, definition.variableType, rootType))
            .orElse(definition.defaultValue.map(IO.succeed(_)))

        (value, definition.variableType.nonNull) match {
          case (None, true)  => IO.fail(ValidationError("null for non-null", ""))
          case (None, false) => IO.succeed(coercedValues)
          case (Some(v), _)  =>
            v.map(value => coercedValues + (definition.name -> value))
        }
      }
    }.map(coercedValues => req.copy(variables = Some(coercedValues)))
  }

  // https://spec.graphql.org/June2018/#IsInputType()
  private def isInputType(t: Type, rootType: RootType): Either[String, Unit] =
    t match {
      case NamedType(name, nonNull)  =>
        rootType.types
          .get(name)
          .map { t =>
            isInputType(t).left
              .map(_ => s"'${t.toString}' is not an input type")
          }
          .getOrElse({ Left(s"'${t.toString}' is not an input type") })
      case ListType(ofType, nonNull) =>
        isInputType(ofType, rootType).left.map(_ => s"'${t.toString}' is not an input type")
    }

  private def isInputType(t: __Type): Either[__Type, Unit] = {
    import __TypeKind._
    t.kind match {
      case LIST | NON_NULL              =>
        t.ofType.fold[Either[__Type, Unit]](Left(t))(isInputType)
      case SCALAR | ENUM | INPUT_OBJECT => Right(())
      case other                        => Left(t)
    }
  }

  private def rewriteValues(value: InputValue, `type`: Type, rootType: RootType): IO[ValidationError, InputValue] =
    `type` match {
      case ListType(ofType, _) =>
        value match {
          case ListValue(values) =>
            IO.foreach(values)(v => rewriteValues(v, ofType, rootType)).map(ListValue(_))
          case _                 => rewriteValues(value, ofType, rootType)
        }
      case NamedType(name, _)  =>
        rootType.types.get(name).map(t => coerceValues(value, t, rootType)).getOrElse(IO.succeed(value))
    }

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  // Same thing with Int into Float.
  private def coerceValues(
    value: InputValue,
    typ: __Type,
    rootType: RootType
  ): IO[ValidationError, InputValue] =
    typ.kind match {
      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val defs = typ.inputFields.getOrElse(List.empty)
            IO.foreach(fields) { case (k, v) =>
              defs
                .find(_.name == k)
                .map(field => coerceValues(v, field.`type`(), rootType).map(k -> _))
                .getOrElse(IO.succeed(k -> value))
            }.map(InputValue.ObjectValue(_))
          case _                              =>
            IO.fail(ValidationError(s"cannot coerce ${typ.kind} to INPUT_OBJECT", ""))
        }

      case __TypeKind.LIST =>
        value match {
          case ListValue(values) =>
            typ.ofType
              .map(innerType => IO.foreach(values)(coerceValues(_, innerType, rootType)).map(ListValue(_)))
              .getOrElse(IO.succeed(value))
          case _                 => IO.fail(ValidationError(s"cannot coerce ${typ.kind} to LIST", ""))
        }

      case __TypeKind.NON_NULL =>
        value match {
          case NullValue => IO.fail(ValidationError("${value} is null, expected NON-NULL", ""))
          case _         =>
            typ.ofType
              .map(innerType => coerceValues(value, innerType, rootType))
              .getOrElse(IO.succeed(value))
        }

      case __TypeKind.ENUM                                 =>
        value match {
          case StringValue(value) => IO.succeed(Value.EnumValue(value))
          case _                  => IO.fail(ValidationError("expected string value", ""))
        }
      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case IntValue.IntNumber(value)    => IO.succeed(Value.FloatValue(value.toDouble))
          case IntValue.LongNumber(value)   => IO.succeed(Value.FloatValue(value.toDouble))
          case IntValue.BigIntNumber(value) => IO.succeed(Value.FloatValue(BigDecimal(value)))
          case _                            => IO.fail(ValidationError("expected number", ""))
        }
      case _                                               =>
        IO.succeed(value)
    }
}
