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
import zio.prelude.EReader
import zio.prelude.fx.ZPure

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
  )(implicit trace: Trace): IO[ValidationError, GraphQLRequest] =
    coerceVariables(req.variables.getOrElse(Map.empty), doc, rootType, skipValidation)
      .map(m => req.copy(variables = Some(m)))

  def coerceVariables(
    variables: Map[String, InputValue],
    doc: Document,
    rootType: RootType,
    skipValidation: Boolean
  )(implicit trace: Trace): IO[ValidationError, Map[String, InputValue]] =
    ZIO.fromEither(coerceVariablesEither(variables, doc, rootType, skipValidation))

  def coerceVariablesEither(
    variables: Map[String, InputValue],
    doc: Document,
    rootType: RootType,
    skipValidation: Boolean
  ): Either[ValidationError, Map[String, InputValue]] = {
    // Scala 2's compiler loves inferring `ZPure.succeed` as ZPure[Nothing, Nothing, Any, R, E, A] so we help it out
    type F[+A] = EReader[Any, ValidationError, A]

    val variableDefinitions = doc.operationDefinitions.flatMap(_.variableDefinitions)

    if (variableDefinitions.isEmpty) Right(variables)
    else
      variableDefinitions
        .foldLeft[F[List[(String, InputValue)]]](ZPure.succeed(Nil)) { case (coercedValues, definition) =>
          val variableName = definition.name
          ZPure.unless[Nothing, Unit, Any, ValidationError, Unit](skipValidation)(
            ZPure
              .fromEither(isInputType(definition.variableType, rootType))
              .mapError(e =>
                ValidationError(
                  s"Type of variable '$variableName' $e",
                  "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
                )
              )
          ) *> {
            variables
              .get(definition.name)
              .map[F[InputValue]](inputValue =>
                rewriteValues(
                  inputValue,
                  definition.variableType,
                  rootType,
                  s"Variable '$variableName'"
                ).catchSome { case _ if skipValidation => ZPure.succeed(inputValue) }
              )
              .orElse(definition.defaultValue.map[F[InputValue]](ZPure.succeed)) match {
              case Some(v)                                                 =>
                for {
                  values <- coercedValues
                  value  <- v
                } yield (definition.name -> value) :: values
              case _ if definition.variableType.nullable || skipValidation => coercedValues
              case _                                                       =>
                ZPure.fail(
                  ValidationError(
                    s"Variable '$variableName' is null but is specified to be non-null.",
                    "The value of a variable must be compatible with its type."
                  )
                )
            }
          }
        }
        .map(_.toMap)
        .runEither
  }

  // https://spec.graphql.org/June2018/#IsInputType()
  private def isInputType(t: Type, rootType: RootType): Either[String, Unit] =
    t match {
      case NamedType(name, _)  =>
        rootType.types
          .get(name)
          .map { t =>
            isInputType(t).left
              .map(_ => s"is not a valid input type.")
          }
          .getOrElse({ Left(s"is not a valid input type.") })
      case ListType(ofType, _) =>
        isInputType(ofType, rootType).left.map(_ => s"is not a valid input type.")
    }

  private def isInputType(t: __Type): Either[__Type, Unit] = {
    import __TypeKind._
    t.kind match {
      case LIST | NON_NULL              =>
        t.ofType.fold[Either[__Type, Unit]](Left(t))(isInputType)
      case SCALAR | ENUM | INPUT_OBJECT => Right(())
      case _                            => Left(t)
    }
  }

  private def rewriteValues(
    value: InputValue,
    `type`: Type,
    rootType: RootType,
    context: => String
  ): EReader[Any, ValidationError, InputValue] =
    resolveType(rootType, `type`) match {
      case Some(typ) => coerceValues(value, typ, context)
      case None      => ZPure.succeed(value)
    }

  private def resolveType(rootType: RootType, `type`: Type): Option[__Type] =
    `type` match {
      case NamedType(name, _)  => rootType.types.get(name)
      case ListType(ofType, _) =>
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
    context: => String // Careful not to materialize unless we need to fail!
  ): EReader[Any, ValidationError, InputValue] =
    typ.kind match {
      case __TypeKind.NON_NULL                     =>
        value match {
          case NullValue =>
            ZPure.fail(
              ValidationError(
                s"$context $value is null, should be ${typ.toType(true)}",
                "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
              )
            )
          case _         =>
            typ.ofType
              .map(innerType => ZPure.suspend(coerceValues(value, innerType, context)))
              .getOrElse(ZPure.succeed(value))
        }

      // Break early
      case _ if value.isInstanceOf[NullValue.type] =>
        ZPure.succeed(NullValue)

      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val defs = typ.allInputFields
            ZPure
              .foreach(fields: Iterable[(String, InputValue)]) { case (k, v) =>
                defs
                  .find(_.name == k)
                  .map(field => coerceValues(v, field._type, s"$context at field '${field.name}'").map(k -> _))
                  .getOrElse {
                    ZPure.fail(ValidationError(s"$context field '$k' does not exist", coercionDescription))
                  }
              }
              .map(l => InputValue.ObjectValue(l.toMap))
          case v                              =>
            ZPure.fail(
              ValidationError(
                s"$context cannot coerce $v to ${typ.name.getOrElse("Input Object")}",
                coercionDescription
              )
            )
        }

      case __TypeKind.LIST =>
        typ.ofType match {
          case None           => ZPure.succeed(value)
          case Some(itemType) =>
            value match {
              case ListValue(values) =>
                ZPure
                  .foreach(values.zipWithIndex) { case (value, i) =>
                    coerceValues(value, itemType, s"$context at index '$i'")
                  }
                  .map(ListValue(_))
              case v                 =>
                ZPure.suspend(coerceValues(v, itemType, context).map(iv => ListValue(List(iv))))
            }
        }

      case __TypeKind.ENUM =>
        value match {
          case StringValue(value) => ZPure.succeed(Value.EnumValue(value))
          case v                  =>
            ZPure.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into ${typ.toType(false)}.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("String") =>
        value match {
          case v: StringValue => ZPure.succeed(v)
          case v              =>
            ZPure.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into String.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Boolean") =>
        value match {
          case v: BooleanValue => ZPure.succeed(v)
          case v               =>
            ZPure.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Boolean.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Int") =>
        value match {
          case v: IntValue => ZPure.succeed(v)
          case v           =>
            ZPure.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Int.",
                coercionDescription
              )
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case v: FloatValue                => ZPure.succeed(v)
          case IntValue.IntNumber(value)    => ZPure.succeed(Value.FloatValue(value.toDouble))
          case IntValue.LongNumber(value)   => ZPure.succeed(Value.FloatValue(value.toDouble))
          case IntValue.BigIntNumber(value) => ZPure.succeed(Value.FloatValue(BigDecimal(value)))
          case v                            =>
            ZPure.fail(
              ValidationError(
                s"$context with value $v cannot be coerced into Float.",
                coercionDescription
              )
            )
        }
      case _                                               =>
        ZPure.succeed(value)
    }
}
