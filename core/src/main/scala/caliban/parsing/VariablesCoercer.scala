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
import zio._
import zio.prelude.EReader
import zio.prelude.fx.ZPure

import scala.collection.compat._

object VariablesCoercer {

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
            isInputType(definition.variableType, rootType) match {
              case Left(e) =>
                failValidation(
                  s"Type of variable '$variableName' $e",
                  "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
                )
              case _       => ZPure.unit
            }
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
                failValidation(
                  s"Variable '$variableName' is null but is specified to be non-null.",
                  "The value of a variable must be compatible with its type."
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
          .map(isInputType(_).left.map(_ => "is not a valid input type."))
          .getOrElse(Left("is not a valid input type."))
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
            failValidation(
              s"$context $value is null, should be ${typ.toType(true)}",
              "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
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
            foreachObjectField(fields) { (k, v) =>
              defs
                .find(_.name == k)
                .map(field => coerceValues(v, field._type, s"$context at field '${field.name}'"))
                .getOrElse(failValidation(s"$context field '$k' does not exist", coercionDescription))
            }
          case v                              =>
            failValidation(
              s"$context cannot coerce $v to ${typ.name.getOrElse("Input Object")}",
              coercionDescription
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
                  .map(ListValue.apply)
              case v                 =>
                ZPure.suspend(coerceValues(v, itemType, context).map(iv => ListValue(List(iv))))
            }
        }

      case __TypeKind.ENUM =>
        value match {
          case StringValue(value) => ZPure.succeed(Value.EnumValue(value))
          case v                  =>
            failValidation(
              s"$context with value $v cannot be coerced into ${typ.toType()}.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("String") =>
        value match {
          case v: StringValue => ZPure.succeed(v)
          case v              =>
            failValidation(
              s"$context with value $v cannot be coerced into String.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Boolean") =>
        value match {
          case v: BooleanValue => ZPure.succeed(v)
          case v               =>
            failValidation(
              s"$context with value $v cannot be coerced into Boolean.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Int") =>
        value match {
          case v: IntValue.IntNumber    => ZPure.succeed(v)
          case v: IntValue.LongNumber   => ZPure.succeed(v)
          case v: IntValue.BigIntNumber => ZPure.succeed(v)
          case v                        =>
            failValidation(
              s"$context with value $v cannot be coerced into Int.",
              coercionDescription
            )
        }

      case __TypeKind.SCALAR if typ.name.contains("Float") =>
        value match {
          case v: FloatValue.FloatNumber      => ZPure.succeed(v)
          case v: FloatValue.DoubleNumber     => ZPure.succeed(v)
          case v: FloatValue.BigDecimalNumber => ZPure.succeed(v)
          case v: IntValue.IntNumber          => ZPure.succeed(Value.FloatValue(v.value.toDouble))
          case v: IntValue.LongNumber         => ZPure.succeed(Value.FloatValue(v.value.toDouble))
          case v: IntValue.BigIntNumber       => ZPure.succeed(Value.FloatValue(BigDecimal(v.value)))
          case v                              =>
            failValidation(
              s"$context with value $v cannot be coerced into Float.",
              coercionDescription
            )
        }
      case _                                               =>
        ZPure.succeed(value)
    }

  private val emptyObjectValue =
    ZPure.succeed[Unit, InputValue.ObjectValue](InputValue.ObjectValue(Map.empty))

  private def foreachObjectField(
    in: Map[String, InputValue]
  )(
    f: (String, InputValue) => EReader[Any, ValidationError, InputValue]
  ): EReader[Any, ValidationError, InputValue.ObjectValue] =
    if (in.isEmpty) emptyObjectValue
    else if (in.size == 1) {
      val (k, v) = in.head
      f(k, v).map(v => InputValue.ObjectValue(Map(k -> v)))
    } else
      ZPure.suspend {
        type Out = EReader[Any, ValidationError, InputValue.ObjectValue]

        val iterator = in.iterator
        val builder  = Map.newBuilder[String, InputValue]

        lazy val recurse: (String, InputValue) => Out = { (k, v) =>
          builder += ((k, v))
          loop()
        }

        def loop(): Out =
          if (iterator.hasNext) {
            val (k, v) = iterator.next()
            f(k, v).flatMap(recurse(k, _))
          } else ZPure.succeed(InputValue.ObjectValue(builder.result()))

        loop()
      }
}
