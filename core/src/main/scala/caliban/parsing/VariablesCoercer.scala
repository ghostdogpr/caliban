package caliban.parsing

import caliban.CalibanError.ValidationError
import caliban.InputValue.ListValue
import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt._
import caliban.schema.RootType
import caliban.{ GraphQLRequest, InputValue, Value }
import zio._

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
    rootType: RootType,
    skipValidation: Boolean
  )(implicit trace: Trace): IO[ValidationError, GraphQLRequest] = {
    val variableDefinitions = doc.operationDefinitions.flatMap(_.variableDefinitions)
    val variables           = req.variables.getOrElse(Map.empty)

    ZIO
      .foldLeft(variableDefinitions)(Map.empty[String, InputValue]) { case (coercedValues, definition) =>
        val variableName = definition.name
        ZIO
          .fromEither(isInputType(definition.variableType, rootType))
          .mapError(e =>
            ValidationError(
              s"Type of variable '$variableName' $e",
              "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
            )
          )
          .unless(skipValidation) *> {
          val value =
            variables
              .get(definition.name)
              .map(inputValue =>
                rewriteValues(
                  inputValue,
                  definition.variableType,
                  rootType,
                  s"Variable '$variableName'"
                ).catchSome { case _ if skipValidation => ZIO.succeed(inputValue) }
              )
              .orElse(definition.defaultValue.map(ZIO.succeed(_)))

          (value, definition.variableType.nullable) match {
            case (None, nullable) =>
              if (skipValidation || nullable) ZIO.succeed(coercedValues)
              else
                ZIO.fail(
                  ValidationError(
                    s"Variable '$variableName' is null but is specified to be non-null.",
                    "The value of a variable must be compatible with its type."
                  )
                )
            case (Some(v), _)     => v.map(value => coercedValues + (definition.name -> value))
          }
        }
      }
      .map(coercedValues => req.copy(variables = Some(coercedValues)))
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
    resolveType(rootType, `type`) match {
      case Some(typ) => coerceValues(value, typ, context)
      case None      => ZIO.succeed(value)
    }

  private def resolveType(rootType: RootType, `type`: Type): Option[__Type] =
    `type` match {
      case NamedType(name, nonNull)  => rootType.types.get(name)
      case ListType(ofType, nonNull) =>
        Some(
          __Type(
            kind = __TypeKind.LIST,
            ofType = resolveType(rootType, ofType)
          )
        )
    }

  private val coercionDescription = "Variable values need to follow GraphQL's coercion rules."

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  // Same thing with Int into Float.
  private def coerceValues(
    value: InputValue,
    typ: __Type,
    context: String
  ): IO[ValidationError, InputValue] =
    typ.kind match {
      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val defs = typ.inputFields.getOrElse(List.empty)
            ZIO
              .foreach(fields) { case (k, v) =>
                defs
                  .find(_.name == k)
                  .map(field => coerceValues(v, field.`type`(), s"$context at field '${field.name}'").map(k -> _))
                  .getOrElse {
                    ZIO.fail(ValidationError(s"$context field '$k' does not exist", coercionDescription))
                  }
              }
              .map(InputValue.ObjectValue(_))
          case NullValue                      => ZIO.succeed(NullValue)
          case v                              =>
            ZIO.fail(
              ValidationError(
                s"$context cannot coerce $v to ${typ.name.getOrElse("Input Object")}",
                coercionDescription
              )
            )
        }

      case __TypeKind.LIST =>
        typ.ofType match {
          case None           => ZIO.succeed(value)
          case Some(itemType) =>
            value match {
              case NullValue         => ZIO.succeed(NullValue)
              case ListValue(values) =>
                ZIO
                  .foreach(values.zipWithIndex) { case (value, i) =>
                    coerceValues(value, itemType, s"$context at index '$i'")
                  }
                  .map(ListValue(_))
              case v                 =>
                coerceValues(v, itemType, context).map(iv => ListValue(List(iv)))
            }
        }

      case __TypeKind.NON_NULL =>
        value match {
          case NullValue =>
            ZIO.fail(
              ValidationError(
                s"$context $value is null, should be ${typ.toType(true)}",
                "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
              )
            )
          case _         =>
            typ.ofType
              .map(innerType => coerceValues(value, innerType, context))
              .getOrElse(ZIO.succeed(value))
        }

      case __TypeKind.ENUM =>
        value match {
          case StringValue(value) => ZIO.succeed(Value.EnumValue(value))
          case NullValue          => ZIO.succeed(NullValue)
          case v                  =>
            ZIO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into ${typ.toType(false)}.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("String") =>
        value match {
          case NullValue      => ZIO.succeed(NullValue)
          case v: StringValue => ZIO.succeed(v)
          case v              =>
            ZIO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into String.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Boolean") =>
        value match {
          case NullValue       => ZIO.succeed(NullValue)
          case v: BooleanValue => ZIO.succeed(v)
          case v               =>
            ZIO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Boolean.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Int") =>
        value match {
          case NullValue   => ZIO.succeed(NullValue)
          case v: IntValue => ZIO.succeed(v)
          case v           =>
            ZIO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Int.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case NullValue                    => ZIO.succeed(NullValue)
          case v: FloatValue                => ZIO.succeed(v)
          case IntValue.IntNumber(value)    => ZIO.succeed(Value.FloatValue(value.toDouble))
          case IntValue.LongNumber(value)   => ZIO.succeed(Value.FloatValue(value.toDouble))
          case IntValue.BigIntNumber(value) => ZIO.succeed(Value.FloatValue(BigDecimal(value)))
          case v                            =>
            ZIO.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Float.",
                coercionDescription
              )
            )
        }
      case _                                               =>
        ZIO.succeed(value)
    }
}
