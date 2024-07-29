package caliban.parsing

import caliban.CalibanError.ValidationError
import caliban.InputValue.ListValue
import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt._
import caliban.schema.RootType
import caliban.validation.Validator.failValidation
import caliban.{ GraphQLRequest, InputValue, Value }

import scala.collection.compat._

object VariablesCoercer {
  import caliban.validation.ValidationOps._

  def coerceVariables(
    req: GraphQLRequest,
    doc: Document,
    rootType: RootType,
    skipValidation: Boolean
  ): Either[ValidationError, GraphQLRequest] =
    coerceVariables(req.variables.getOrElse(Map.empty), doc, rootType, skipValidation)
      .map(m => req.copy(variables = Some(m)))

  def coerceVariables(
    variables: Map[String, InputValue],
    doc: Document,
    rootType: RootType,
    skipValidation: Boolean
  ): Either[ValidationError, Map[String, InputValue]] =
    try
      coerceVariablesUnsafe(variables, doc, rootType, skipValidation)
    catch {
      case _: StackOverflowError => Left(ValidationError("max arguments depth exceeded", ""))
    }

  private def coerceVariablesUnsafe(
    variables: Map[String, InputValue],
    doc: Document,
    rootType: RootType,
    skipValidation: Boolean
  ): Either[ValidationError, Map[String, InputValue]] = {
    // Scala 2's compiler loves inferring `ZPure.succeed` as ZPure[Nothing, Nothing, Any, R, E, A] so we help it out
    type F[+A] = Either[ValidationError, A]

    val variableDefinitions = doc.operationDefinitions.flatMap(_.variableDefinitions)

    if (variableDefinitions.isEmpty) Right(variables)
    else
      variableDefinitions
        .foldLeft[F[List[(String, InputValue)]]](Right(Nil)) { case (coercedValues, definition) =>
          val variableName = definition.name
          when(!skipValidation)(
            isInputType(definition.variableType, rootType) match {
              case Left(e) =>
                failValidation(
                  s"Type of variable '$variableName' $e",
                  "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
                )
              case _       => unit
            }
          ) *> {
            variables
              .get(definition.name)
              .map { inputValue =>
                rewriteValues(
                  inputValue,
                  definition.variableType,
                  rootType,
                  s"Variable '$variableName'"
                ) match {
                  case Right(v)                  => Right(v)
                  case Left(_) if skipValidation => Right(inputValue)
                  case Left(e)                   => Left(e)
                }
              }
              .orElse(definition.defaultValue.map(Right(_))) match {
              case Some(v)                                                 =>
                for {
                  values <- coercedValues
                  value  <- v
                } yield (definition.name -> value) :: values
              case _ if definition.variableType.nullable || skipValidation => coercedValues
              case _                                                       =>
                failValidation(
                  s"Variable '$variableName' is null but is specified to be non-null.",
                  "The value of a variable must be compatible with its type."
                )
            }
          }
        }
        .map(_.toMap)
  }

  // https://spec.graphql.org/June2018/#IsInputType()
  private def isInputType(t: Type, rootType: RootType): Either[String, Unit] =
    t match {
      case NamedType(name, _)  =>
        rootType.types.getOrElse(name, null) match {
          case null => Left("is not a valid input type.")
          case t    => isInputType(t).left.map(_ => "is not a valid input type.")
        }
      case ListType(ofType, _) =>
        isInputType(ofType, rootType).left.map(_ => "is not a valid input type.")
    }

  private def isInputType(t: __Type): Either[__Type, Unit] = {
    import __TypeKind._
    t.kind match {
      case LIST | NON_NULL              => t.ofType.fold[Either[__Type, Unit]](Left(t))(isInputType)
      case SCALAR | ENUM | INPUT_OBJECT => Right(())
      case _                            => Left(t)
    }
  }

  private def rewriteValues(
    value: InputValue,
    `type`: Type,
    rootType: RootType,
    context: => String
  ): Either[ValidationError, InputValue] =
    resolveType(rootType, `type`) match {
      case Some(typ) => coerceValues(value, typ, context)
      case None      => Right(value)
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
  ): Either[ValidationError, InputValue] =
    typ.kind match {
      case __TypeKind.NON_NULL =>
        value match {
          case NullValue =>
            failValidation(
              s"$context $value is null, should be ${typ.toType(true)}",
              "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
            )
          case _         =>
            typ.ofType match {
              case Some(innerType) => coerceValues(value, innerType, context)
              case _               => Right(value)
            }
        }

      // Break early
      case _ if value.isInstanceOf[NullValue.type] =>
        Right(NullValue)

      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val defs = typ.allInputFields
            foreachObjectField(fields) { (k, v) =>
              defs.find(_.name == k) match {
                case Some(field) => coerceValues(v, field._type, s"$context at field '${field.name}'")
                case _           => failValidation(s"$context field '$k' does not exist", coercionDescription)
              }
            }
          case v                              =>
            failValidation(
              s"$context cannot coerce $v to ${typ.name.getOrElse("Input Object")}",
              coercionDescription
            )
        }

      case __TypeKind.LIST =>
        typ.ofType match {
          case None           => Right(value)
          case Some(itemType) =>
            value match {
              case ListValue(values) =>
                validateAll(values.zipWithIndex) { case (value, i) =>
                  coerceValues(value, itemType, s"$context at index '$i'")
                }.map(ListValue.apply)
              case v                 =>
                coerceValues(v, itemType, context).map(iv => ListValue(List(iv)))
            }
        }

      case __TypeKind.ENUM =>
        value match {
          case StringValue(value) => Right(Value.EnumValue(value))
          case v                  =>
            failValidation(
              s"$context with value $v cannot be coerced into ${typ.toType()}.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("String") =>
        value match {
          case v: StringValue => Right(v)
          case v              =>
            failValidation(
              s"$context with value $v cannot be coerced into String.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Boolean") =>
        value match {
          case v: BooleanValue => Right(v)
          case v               =>
            failValidation(
              s"$context with value $v cannot be coerced into Boolean.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Int") =>
        value match {
          case v: IntValue.IntNumber    => Right(v)
          case v: IntValue.LongNumber   => Right(v)
          case v: IntValue.BigIntNumber => Right(v)
          case v                        =>
            failValidation(
              s"$context with value $v cannot be coerced into Int.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case v: FloatValue.FloatNumber      => Right(v)
          case v: FloatValue.DoubleNumber     => Right(v)
          case v: FloatValue.BigDecimalNumber => Right(v)
          case v: IntValue.IntNumber          => Right(Value.FloatValue(v.value.toDouble))
          case v: IntValue.LongNumber         => Right(Value.FloatValue(v.value.toDouble))
          case v: IntValue.BigIntNumber       => Right(Value.FloatValue(BigDecimal(v.value)))
          case v                              =>
            failValidation(
              s"$context with value $v cannot be coerced into Float.",
              coercionDescription
            )
        }
      case _                                               =>
        Right(value)
    }

  private val emptyObjectValue: Either[ValidationError, InputValue.ObjectValue] =
    Right(InputValue.ObjectValue(Map.empty))

  private def foreachObjectField(
    in: Map[String, InputValue]
  )(
    f: (String, InputValue) => Either[ValidationError, InputValue]
  ): Either[ValidationError, InputValue.ObjectValue] =
    in.size match {
      case 0 => emptyObjectValue
      case 1 =>
        val (k, v) = in.head
        f(k, v).map(v => InputValue.ObjectValue(Map(k -> v)))
      case _ =>
        val it      = in.iterator
        val builder = Map.newBuilder[String, InputValue]
        var err     = null.asInstanceOf[ValidationError]

        while (it.hasNext && (err eq null)) {
          val (k, v) = it.next()
          f(k, v) match {
            case Right(v1) => builder += ((k, v1))
            case Left(e)   => err = e
          }
        }
        if (err eq null) Right(InputValue.ObjectValue(builder.result()))
        else Left(err)
    }
}
