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

object VariablesCoercer {
  private val primitiveTypes: List[__Type] = List(
    __Type(kind = __TypeKind.SCALAR, name = Some("Boolean")),
    __Type(kind = __TypeKind.SCALAR, name = Some("Int")),
    __Type(kind = __TypeKind.SCALAR, name = Some("Float")),
    __Type(kind = __TypeKind.SCALAR, name = Some("String"))
  )

  def coerceVariables(
    req: GraphQLRequest,
    doc: Document,
    rootType: RootType
  ): IO[ValidationError, GraphQLRequest] = {
    val variableDefinitions    = doc.operationDefinitions.flatMap(_.variableDefinitions)
    var variables              = req.variables.getOrElse(Map.empty)
    val rootTypeWithPrimitives = rootType.copy(additionalTypes = rootType.additionalTypes ++ primitiveTypes)

    IO.foldLeft(variableDefinitions)(Map.empty[String, InputValue]) { case (coercedValues, definition) =>
      val variableName = definition.name
      IO.fromEither(isInputType(definition.variableType, rootTypeWithPrimitives))
        .mapError(e =>
          ValidationError(
            s"Type of variable '$variableName' $e",
            "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
          )
        ) *> {
        val value =
          variables
            .get(definition.name)
            .map(rewriteValues(_, definition.variableType, rootTypeWithPrimitives, s"Variable '$variableName'"))
            .orElse(definition.defaultValue.map(IO.succeed(_)))

        (value, definition.variableType.nonNull) match {
          case (None, true)  =>
            IO.fail(
              ValidationError(
                s"Variable '$variableName' is null but is specified to be non-null.",
                "The value of a variable must be compatible with its type."
              )
            )
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
              .map(_ => s"is not a valid input type.")
          }
          .getOrElse({ Left(s"is not a valid input type.") })
      case ListType(ofType, nonNull) =>
        isInputType(ofType, rootType).left.map(_ => s"is not a valid input type.")
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

  private def rewriteValues(
    value: InputValue,
    `type`: Type,
    rootType: RootType,
    context: String
  ): IO[ValidationError, InputValue] =
    `type` match {
      case ListType(ofType, _) =>
        value match {
          case ListValue(values) =>
            IO.foreach(values)(v => rewriteValues(v, ofType, rootType, context)).map(ListValue(_))
          case _                 => rewriteValues(value, ofType, rootType, context)
        }
      case NamedType(name, _)  =>
        rootType.types.get(name).map(t => coerceValues(value, t, rootType, context)).getOrElse(IO.succeed(value))
    }

  private val coercionDescription = "Variable values need to follow GraphQL's coercion rules."

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  // Same thing with Int into Float.
  private def coerceValues(
    value: InputValue,
    typ: __Type,
    rootType: RootType,
    context: String
  ): IO[ValidationError, InputValue] =
    typ.kind match {
      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val defs = typ.inputFields.getOrElse(List.empty)
            IO.foreach(fields) { case (k, v) =>
              defs
                .find(_.name == k)
                .map(field => coerceValues(v, field.`type`(), rootType, context).map(k -> _))
                .getOrElse(IO.succeed(k -> value))
            }.map(InputValue.ObjectValue(_))
          case NullValue                      => IO.succeed(NullValue)
          case v                              =>
            IO.fail(
              ValidationError(
                s"$context cannot coerce $v to INPUT_OBJECT",
                coercionDescription
              )
            )
        }

      case __TypeKind.LIST =>
        value match {
          case ListValue(values) =>
            typ.ofType
              .map(innerType => IO.foreach(values)(coerceValues(_, innerType, rootType, context)).map(ListValue(_)))
              .getOrElse(IO.succeed(value))
          case NullValue         => IO.succeed(NullValue)
          case v                 =>
            IO.fail(
              ValidationError(
                s"$context with value $v cannot coerced into ${typ.toType(false)}",
                coercionDescription
              )
            )
        }

      case __TypeKind.NON_NULL =>
        value match {
          case NullValue =>
            IO.fail(
              ValidationError(
                s"$context $value is null, should be ${typ.toType(true)}",
                "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
              )
            )
          case _         =>
            typ.ofType
              .map(innerType => coerceValues(value, innerType, rootType, context))
              .getOrElse(IO.succeed(value))
        }

      case __TypeKind.ENUM =>
        value match {
          case StringValue(value) => IO.succeed(Value.EnumValue(value))
          case NullValue          => IO.succeed(NullValue)
          case v                  =>
            IO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into into ${typ.toType(false)}.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("String") =>
        value match {
          case NullValue      => IO.succeed(NullValue)
          case v: StringValue => IO.succeed(v)
          case v              =>
            IO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into String.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Boolean") =>
        value match {
          case NullValue       => IO.succeed(NullValue)
          case v: BooleanValue => IO.succeed(v)
          case v               =>
            IO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Boolean.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Int") =>
        value match {
          case NullValue   => IO.succeed(NullValue)
          case v: IntValue => IO.succeed(v)
          case v           =>
            IO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Int.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case NullValue                    => IO.succeed(NullValue)
          case IntValue.IntNumber(value)    => IO.succeed(Value.FloatValue(value.toDouble))
          case IntValue.LongNumber(value)   => IO.succeed(Value.FloatValue(value.toDouble))
          case IntValue.BigIntNumber(value) => IO.succeed(Value.FloatValue(BigDecimal(value)))
          case v                            =>
            IO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Float.",
                coercionDescription
              )
            )
        }
      case _                                               =>
        IO.succeed(value)
    }
}
